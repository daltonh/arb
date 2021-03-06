# Rxntoarb::Rxn
# (C) Copyright Christian Biscombe 2016-2019

require 'set'
require_relative 'reaction'

module Rxntoarb

  class Rxn

    attr_accessor :aliases, :bounding_regions, :check_units, :current_surface_regions, :current_volume_regions, :file, :header, :initial_species, :labels, :parameters, :par_units, :parent_label, :parent_parameters, :reactions, :species, :surface_regions, :volume_regions, :volume_species

    def initialize(file) #{{{
      @aliases = {}
      @bounding_regions = Hash.new(Set.new) # key is volume_region, value is array of all surface_regions that bound that volume_region
      @check_units = nil
      @current_surface_regions = Set.new([nil])
      @current_volume_regions = Set.new([nil])
      @file = file
      @header = []
      @initial_species = Set.new # elements are species.tags
      @labels = Set.new
      @parameters = Set.new
      @par_units = {}
      @parent_label = ''
      @parent_parameters = nil
      @reactions = Set.new
      @replacements = {}
      @species = Set.new
      @surface_regions = Set.new
      @volume_regions = Set.new
      @volume_species = Hash.new(Set.new) # key is volume_region, value is array of all species defined on that volume_region
    end #}}}

    def parse #{{{
      warn "#{'*'*200}\nINFO: parsing input file #{@file}" if Rxntoarb.options[:debug]
      File.foreach(@file) do |line|
        catch :next_line do
          @replacements.each { |sub, rep| line.gsub!(sub, rep) }
          if Rxntoarb.options[:debug]
            warn '='*200
            line_no = $.
            Rxntoarb.print_debug(:line_no, :line){}
          end
          case line
          # Skip full-line comments and blank lines
          when /^\s*(#|$)/
            next

          # Comment lines to be retained in the header start with !
          when /^\s*!/
            @header << line.sub('!', '#').chomp

          # Command-line options can also be specified in the input file (using exactly the same syntax)
          # These take precedence over command-line options
          when /^\s*options\s+([^#]+)/i
            Rxntoarb.optparse($1.split)

          # Replacement definition
          when /^\s*substitute\s+(\S+|\/.*?(?<!\\)\/i?)\s+([^#]*)/i
            @replacements[regexpify($1)] = $2.strip

          # Include/exclude lines based on string/regexp match. Maximum of one include and one exclude statement allowed
          when /^\s*(include_only|exclude)\s+(\S+|\/.*?(?<!\\)\/i?)/i
            Rxntoarb.options[:keep] ||= {}
            keep = Rxntoarb.options[:keep][$1.to_sym] || Regexp.union
            Rxntoarb.options[:keep][$1.to_sym] = Regexp.union(keep, regexpify($2))

          # Define surface or volume regions
          when /^\s*(surface|volume)_regions?\s+([^#]+)/i
            Rxntoarb.options[:none_centred] = false unless Rxntoarb.options[:none_centred] == :flag # not none_centred unless -n flag given on command line
            location = $1.downcase
            eval "@current_#{location}_regions = []"
            $2.scan(/\w+|<[^>]+>/) do |region| # angle brackets optional unless region name contains spaces or special characters
              if exclude?("@#{region}") # region excluded by include_only or exclude statement
                warn "INFO: region #{region} excluded due to include_only or exclude statement" if Rxntoarb.options[:debug]
                next
              end
              region = Rxntoarb.debracket(region)
              eval "@current_#{location}_regions << region"
              eval "@#{location}_regions << region"
            end

          # Species present initially determine the magnitudes of all other species
          when /^\s*initial_species\s+([^#]+)/i
            $1.scan(/(?<species>[^@]+)(?:@(?<region>\w+|<[^>]+>|(?<array>\[((?>[^\[\]]+)|\g<array>)*\])))?,?\s*/) do |species_list, region_list|
              species_list = Rxntoarb.arrayify(species_list[/\A\[?(.*?)\]?\Z/, 1])
              region_list &&= Rxntoarb.arrayify(region_list[/\A\[?(.*?)\]?\Z/, 1])
              region_list ||= [nil]
              region_list.each do |region|
                species_list.each do |species|
                  species = Rxntoarb.debracket(species)
                  @initial_species << "#{species}#{"@#{region}" if region}"
                end
              end
            end

          # Reaction (or syntax error)
          else
            line_includes_surface_region = line =~ /[^#]*?@s\b/
            line_includes_volume_region = line =~ /[^#]*?@v\b/
            unless Rxntoarb.options[:none_centred] == :flag
              raise 'missing surface_region definition for reaction' if @current_surface_regions == [nil] && line_includes_surface_region
              raise 'missing volume_region definition for reaction' if @current_volume_regions == [nil] && line_includes_volume_region
            end
            @current_volume_regions.each do |volume_region|
              @current_surface_regions.each do |surface_region|
                @bounding_regions[volume_region] << surface_region if surface_region && volume_region && line_includes_surface_region && line_includes_volume_region # keep track of surface_regions that bound each volume_region
                rline = line.dup # dup line as substitutions below need to be done for each surface_region and volume_region
                rline.gsub!(/@s\b/, "@#{surface_region}") if surface_region
                rline.gsub!(/@v\b/, "@#{volume_region}") if volume_region
                read_reaction(rline)
                break unless line_includes_surface_region
              end
              break unless line_includes_volume_region
            end
          end

        end
      end
      puts "INFO: read #{@species.size} species, #{@reactions.size} reaction#{'s' unless @reactions.size == 1}, #{@parameters.size} parameter#{'s' unless @parameters.size == 1} from input file #{@file}"
      warn "INFO: parse complete for input file #{@file}\n#{'*'*200}" if Rxntoarb.options[:debug]

    rescue => msg
      error(msg)
    end #}}}

    def error(msg, type = :ERROR) #{{{
      type = :ERROR if Rxntoarb.options[:strict]
      warn "#{type} in #{@file} at line #{$.}: #{msg}"
      if type == :ERROR
        warn msg.backtrace if Rxntoarb.options[:debug]
        abort
      end
    end #}}}

    private

    def regexpify(string) #{{{
      case string 
      when /^\/.*\/i$/i # case insensitive regexp
        Regexp.new(string[1..-3], 'i')
      when /^\/.*\/$/ # case sensitive regexp
        Regexp.new(string[1..-2], nil)
      else # just a string - no special escapes recognised
        Regexp.new(Regexp.escape(string), nil)
      end
    end #}}}

    def exclude?(string) #{{{
      return false unless Rxntoarb.options[:keep]
      exclude = (Rxntoarb.options[:keep][:exclude] && string =~ Rxntoarb.options[:keep][:exclude])
      exclude || (Rxntoarb.options[:keep][:include_only] && string !~ Rxntoarb.options[:keep][:include_only])
    end #}}}

    def read_reaction(rline) #{{{
      excluded = exclude?(rline)
      reaction = Reaction.new(rline, self, excluded)
      @aliases[reaction.aka] ||= reaction.parent_label if reaction.aka
      if excluded # reaction excluded by include_only or exclude statement
        @parent_parameters = reaction.parameters if reaction.parameters # save parameters from parent reaction in case parent is excluded but child is included
        warn 'INFO: reaction excluded due to include_only or exclude statement' if Rxntoarb.options[:debug]
        throw :next_line
      end
      @reactions << reaction
      reaction.all_species.each do |species|
        @species << species
        @volume_species[species.region] << species if species.free? # add species to volume_species hash so we know that a source term is required on all surface_regions that bound this volume_region
      end
      if @parent_parameters
        reaction.parameters = @parent_parameters if reaction.indented # apply parent parameters to first included child if parent reaction has been excluded
        @parent_parameters = nil
      end
      @parameters += reaction.parameters if reaction.parameters
    end #}}}

  end

end

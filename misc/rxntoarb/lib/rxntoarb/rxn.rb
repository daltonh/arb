# Rxntoarb::Rxn
# (C) Copyright Christian Biscombe 2016-2018

require 'set'
require_relative 'reaction'

module Rxntoarb

  class Rxn

    attr_accessor :aliases, :bounding_regions, :check_units, :file, :header, :initial_species, :labels, :parameters, :par_units, :parent_label, :reactions, :species, :surface_regions, :volume_regions, :volume_species

    def initialize(file) #{{{
      @aliases = {}
      @bounding_regions = Hash.new(Set.new) # key is volume_region, value is array of all surface_regions that bound that volume_region
      @check_units = nil
      @file = file
      @header = []
      @initial_species = Set.new # elements are species.tags
      @labels = Set.new
      @parameters = Set.new
      @par_units = {}
      @parent_label = ''
      @reactions = Set.new
      @replacements = {}
      @species = Set.new
      @surface_regions = Set.new
      @volume_regions = Set.new
      @volume_species = Hash.new(Set.new) # key is volume_region, value is array of all species defined on that volume_region
    end #}}}

    def parse #{{{
      surface_region_list = Set.new([nil])
      volume_region_list = Set.new([nil])
      warn "#{'*'*200}\nINFO: parsing input file #{@file}" if Rxntoarb.options[:debug]
      File.foreach(@file) do |line|
        @replacements.each { |sub, rep| line.gsub!(sub, rep) }
        if Rxntoarb.options[:debug]
          warn '='*200
          Rxntoarb.print_debug([:$., :line]){}
        end
        case line
        # Skip full-line comments and blank lines
        when /^\s*(#|$)/
          next

        # Comment lines to be retained in the header start with !
        when /^\s*!/
          @header << line.sub('!', '#').chomp

        # Replacement definition
        when /^\s*let\s+([^#]+)=([^#]+)/i
          @replacements[$1.strip] = $2.strip

        # Include/exclude lines based on regexp match. Maximum of one include and one exclude statement allowed
        when /^\s*(include_only|exclude)\s+\/(.*?)(?<!\\)\/(i)?/i
          Rxntoarb.options[:keep] ||= {}
          Rxntoarb.options[:keep][$1.to_sym] = Regexp.new($2, $3)

        # Define surface or volume regions
        when /^\s*(surface|volume)_regions?\s+([^#]+)/i
          Rxntoarb.options[:none_centred] = false unless Rxntoarb.options[:none_centred] == :flag # not none_centred unless -n flag given on command line
          location = $1.downcase
          eval "#{location}_region_list = []"
          $2.scan(/\w+|<[^>]+>/) do |region| # angle brackets optional unless region name contains spaces or special characters
            if exclude?("@#{region}", :region) # region excluded by include_only or exclude statement
              warn "INFO: region #{region} excluded due to include_only or exclude statement" if Rxntoarb.options[:debug]
              next
            end
            region.tr!('<>', '')
            eval "#{location}_region_list << region"
            eval "@#{location}_regions << region"
          end

        # Species present initially determine the magnitudes of all other species
        when /^\s*initial_species\s+([^#]+)/i
          $1.scan(/(?<species>[^@]+)(?:@(?<region>\w+|<[^>]+>|(?<array>\[((?>[^\[\]]+)|\g<array>)*\])))?,?\s*/) do |species_list, region_list|
            species_list = species_list[/\A\[?(.*?)\]?\Z/, 1].split(/,\s*/)
            region_list &&= region_list[/\A\[?(.*?)\]?\Z/, 1].split(/,\s*/).map { |region| region.tr('<>', '').strip }
            region_list ||= [nil]
            region_list.each do |region|
              species_list.each do |species|
                species.tr!('<>', '')
                @initial_species << "#{species}#{"@#{region}" if region}"
              end
            end
          end

        # Reaction (or syntax error)
        else
          unless Rxntoarb.options[:none_centred] == :flag
            raise 'missing surface_region definition for reaction' if surface_region_list == [nil] and line =~ /[^#]*?@s\b/
            raise 'missing volume_region definition for reaction' if volume_region_list == [nil] and line =~ /[^#]*?@v\b/
          end
          volume_region_list.each do |volume_region|
            surface_region_list.each do |surface_region|
              @bounding_regions[volume_region] << surface_region if surface_region && volume_region # keep track of surface_regions that bound each volume_region
              rline = line.dup # dup line as substitutions below need to be done for each surface_region and volume_region
              rline.gsub!(/@s\b/, "@#{surface_region}") if surface_region
              rline.gsub!(/@v\b/, "@#{volume_region}") if volume_region
              if exclude?(rline) # reaction excluded by include_only or exclude statement
                warn 'INFO: reaction excluded due to include_only or exclude statement' if Rxntoarb.options[:debug]
                next
              end
              begin
                reaction = Reaction.new(rline, self)
                @reactions << reaction
                reaction.all_species.each do |species|
                  @species << species
                  @volume_species[species.region] << species if species.free? # add species to volume_species hash so we know that a source term is required on all surface_regions that bound this volume_region
                end
                @parameters += reaction.parameters if reaction.parameters
                @aliases[reaction.aka] ||= reaction.parent_label if reaction.aka
              ensure
                Rxntoarb.print_debug(:reaction){}
              end
            end
          end
        end
      end
      puts "INFO: read #{@species.size} species, #{@reactions.size} reaction#{'s' unless @reactions.size == 1}, #{@parameters.size} parameter#{'s' unless @parameters.size == 1} from input file #{@file}"
      warn "INFO: parse complete for input file #{@file}\n#{'*'*200}" if Rxntoarb.options[:debug]

    rescue => msg
      error(msg)
    end #}}}

  def error(msg, type=:ERROR) #{{{
    warn "#{type} in #{@file} at line #{$.}: #{msg}"
    if type == :ERROR
      warn msg.backtrace if Rxntoarb.options[:debug]
      abort
    end
  end #}}}

    private

    def exclude?(string, type=:default) #{{{
      return false unless Rxntoarb.options[:keep]
      exclude = false
      Rxntoarb.options[:keep].each do |keep, regexp|
        if type == :region
          next unless regexp.source =~ /\A@/ # first character in regexp must be @ to operate on regions
        end
        if keep == :include_only
          exclude = exclude || string !~ regexp # exclude if string doesn't match regexp
        else
          exclude = exclude || string =~ regexp # exclude if string matches regexp
        end
      end
      exclude
    end #}}}

  end

end

# Rxntoarb::Species
# (C) Copyright Christian Biscombe 2016-2018

module Rxntoarb

  class Species

    attr_reader :bound, :centring, :coeff, :conc, :free, :location, :meta_coeff, :mw, :name, :rate_coeff, :region, :tag, :units, :units_power

    def initialize(species, rxn) #{{{
      match = /\A\s*(\d+)?\s*[*.]?\s*\[?(\d+)?\s*[*.]?\s*([^@]+?)(?:@(\w+|<[^>]+>))?\]?\s*\z/.match(species)
      raise 'syntax error or unrecognised reaction type' unless match
      @coeff, @meta_coeff, @name, @region = match.captures
      @coeff ||= 1
      @coeff = @coeff.to_i
      @meta_coeff ||= 1 # 'metaspecies' are groups of entities that behave as a single entity in reactions, e.g. a cluster of phospholipids comprising a binding site
      @meta_coeff = @meta_coeff.to_i
      @rate_coeff = @coeff*@meta_coeff
      @name = Rxntoarb.debracket(@name)
      regions = rxn.surface_regions + rxn.volume_regions
      unless Rxntoarb.options[:none_centred]
        unless @region
          if regions.size == 1 # if there is only one region, it needn't be specified explicitly
            @region = regions.first.dup
          else
            raise "missing region for species #{@name}"
          end
        end
        @region = Rxntoarb.debracket(@region)
        raise "unknown region #{@region}" unless regions.include?(@region)
      end

      @bound = rxn.surface_regions.include?(@region) # true if species is surface-bound
      @free = rxn.volume_regions.include?(@region) # true if species is in solution
      @tag = "#{@name}#{"@#{@region}" if @region}" # unique identifier of the form 'name@region'
      @conc = "#{@bound ? 's' : 'c'}_#{@tag}" # using 's' for surface concentrations and 'c' for volume (or none_centred) concentrations
      @mw = @bound ? nil : "(#{@name.tr('()[]', '').split(':').map { |component| "+<MW_#{component}>" }.join})" # calculate molecular weight as sum of MWs of components

      @centring, @location, @units, @units_power = if Rxntoarb.options[:none_centred]
                                                     ['NONE', :none] + (rxn.volume_regions.empty? ? ["mol m-2", "mol#{@coeff} m-#{@coeff*2}"] : ["mol m-3", "mol#{@coeff} m-#{@coeff*3}"])
                                                   elsif @bound
                                                     ['FACE', :surface, "mol m-2", "mol#{@coeff} m-#{@coeff*2}"]
                                                   else
                                                     ['CELL', :volume, "mol m-3", "mol#{@coeff} m-#{@coeff*3}"]
                                                   end
    end #}}}

    def ==(other) #{{{
      self.class == other.class && @tag == other.tag # equality of species objects based on tags (name and region) only
    end #}}}

    def hash #{{{
      self.class.hash ^ @tag.hash # equality of species objects based on tags (name and region) only
    end #}}}

    alias bound? bound
    alias free? free
    alias eql? ==
    alias inspect tag

  end

end

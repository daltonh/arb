# Rxntoarb::Species
# (C) Copyright Christian Biscombe 2016-2017
# 2017-10-10

require_relative 'rxn'

module Rxntoarb

  class Species

    INT_ATTR = [:bound, :centring, :conc, :free, :location, :mw, :name, :region, :tag, :units] # intrinsic attributes that determine equality of species objects
    attr_reader *(INT_ATTR + [:coeff, :units_power])

    def initialize(species, rxn) #{{{
      match = /\A\s*(\d+)?\s*[*.]?\s*([^@]+?)(?:@(\w+|<[^>]+>))?\s*\z/.match(species)
      raise 'syntax error or unrecognised reaction type' unless match
      @coeff, @name, @region = match.captures
      @coeff ||= 1
      @coeff = @coeff.to_i
      @name.tr!('<>', '')
      unless Rxntoarb.options[:none_centred]
        unless @region
          if (rxn.surface_regions+rxn.volume_regions).size == 1 # if there is only one region, it needn't be specified explicitly
            @region = (rxn.surface_regions+rxn.volume_regions).first.dup
          else
            raise "missing region for species #{@name}"
          end
        end
        @region.tr!('<>', '')
        raise "unknown region #{@region}" unless (rxn.surface_regions+rxn.volume_regions).include?(@region)
      end

      @bound = rxn.surface_regions.include?(@region) # true if species is surface-bound
      @free = rxn.volume_regions.include?(@region) # true if species is in solution
      @tag = "#{@name}#{"@#{@region}" if @region}" # unique identifier of the form 'name@region'
      @conc = "#{@bound ? 's' : 'c'}_#{@tag}" # using 's' for surface concentrations and 'c' for volume (or none_centred) concentrations
      @mw = @bound ? nil : "(#{@name.tr('()[]', '').split(':').map { |component| "+<MW_#{component}>" }.join})" # calculate molecular weight as sum of MWs of components

      @centring, @location = if Rxntoarb.options[:none_centred]
                               ['NONE', :none]
                             elsif @bound
                               ['FACE', :surface]
                             else
                               ['CELL', :volume]
                             end
      @units, @units_power = if @bound
                               ["mol m-2", "mol#{@coeff} m-#{@coeff*2}"]
                             elsif @free
                               ["mol m-3", "mol#{@coeff} m-#{@coeff*3}"]
                             end
    end #}}}

    def ==(other) #{{{
      self.class == other.class && (INT_ATTR).all? { |attr| eval("@#{attr} == other.#{attr}") } # equality of species objects based on intrinsic attributes only
    end #}}}

    def hash #{{{
      INT_ATTR.map(&:hash).inject(:^) # equality of species objects based on intrinsic attributes only
    end #}}}

    alias bound? bound
    alias free? free
    alias eql? ==
    alias inspect tag

  end

end

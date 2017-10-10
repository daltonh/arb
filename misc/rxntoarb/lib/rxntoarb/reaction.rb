# Rxntoarb::Reaction
# (C) Copyright Christian Biscombe 2016-2017
# 2017-10-10

require_relative 'parameter'
require_relative 'rxn'
require_relative 'rxntoarb'
require_relative 'species'

module Rxntoarb

  class Reaction

    attr_reader :aka, :all_species, :centring, :comment, :enzyme, :indent, :intermediates, :parameters, :parent_label, :products, :rate_units, :reactants, :region, :species_regions, :type
    attr_accessor :label

    def initialize(reaction, rxn) #{{{
      match = /^(\s+)?(?:(.+?):\s+)?(.+?)(?:<=>(.+?)->|{(.+?)}->|(<=>|->))([^;#]+)?;?([^#\n]+)?(#.*)?$/.match(reaction)
      raise 'syntax error or unrecognised reaction type' unless match
      @indent, @aka, reactants, intermediates, enzyme, arrow, products, parameters, @comment = match.captures # parse reaction information into strings; reactants is the only one that is necessarily non-nil

      label = ''
      species_regions = []
      all_species = []
      [reactants, intermediates, enzyme, products].each do |string|
        if string.nil? || string =~ /\A\s*\z/
          all_species << nil
          next
        end
        species_array = []
        string.split(/\s+\+\s+/).each do |term| # spaces around + required so that + can be used in species names (e.g. ions)
          begin
            species = Species.new(term, rxn)
          ensure
            Rxntoarb.print_debug(:species){}
          end
          species_array << species
        end
        label << "|" unless label.empty?
        label << species_array.map(&:tag).join(',')
        species_regions |= species_array.map(&:region)
        all_species << species_array
      end
      @reactants, @intermediates, @enzyme, @products = all_species
      @all_species = all_species.flatten.compact.uniq
      raise "duplicate label #{label}. Most likely there is a duplicate reaction" if rxn.labels.include?(label)
      @label = label
      rxn.labels << @label
      @species_regions = species_regions.compact

      if @indent
        raise 'alias specified for indented reaction' if @aka
        raise 'kinetic parameters specified for indented reaction' if parameters
      else
        raise 'missing kinetic parameters' unless parameters
        rxn.parent_label = @label.dup
      end
      @parent_label = rxn.parent_label.dup # for indented reactions (children), parent_label is the label of the previous non-indented reaction (parent)

      @type = if @intermediates
                :twostep
              elsif @enzyme
                raise 'more than one reactant in Michaelis-Menten reaction' if @reactants.size > 1 || @reactants.map(&:coeff).max > 1
                raise 'more than one enzyme in Michaelis-Menten reaction' if @enzyme.size > 1 || @enzyme.map(&:coeff).max > 1
                :MichaelisMenten
              elsif arrow == '<=>'
                raise 'no products in reversible reaction' unless @products
                :reversible
              else
                :irreversible
              end

      @centring, @region, @rate_units = if Rxntoarb.options[:none_centred] # centring and region relate to the region on which the reaction is occurring
                                          [:NONE, nil, nil]
                                        elsif rxn.surface_regions - @species_regions != rxn.surface_regions # reaction is occurring on surface
                                          [:FACE, (@species_regions - rxn.volume_regions).first, 'mol m-2 s-1']
                                        else  # reaction is occurring in volume
                                          [:CELL, @species_regions.first, 'mol m-3 s-1']
                                        end

      @parameters = nil
      if parameters
        @parameters = []
        rxn.check_units = !@species_regions.empty? # skip check when concentration units are unknown
        parameters.scan(/(\S+)\s*=\s*([-+]?\d+\.?\d*(?:[DdEe][-+]?\d+)?|'[^']*'|"[^"]*")\s*(.*?)?(?:,|$)/) do |name, value, units|
          begin
            parameter = Parameter.new(name, value, units)
          ensure
            Rxntoarb.print_debug(:parameter){}
          end
          @parameters << parameter
          rxn.par_units[parameter.name] = parameter.units # save units for consistency checking
          rxn.check_units = false if parameter.units.nil? # skip check if parameter is defined in terms of a previously defined parameter (i.e. it's a string)
        end

        # Check that all required kinetic parameters are present
        case @type
        when :twostep
          check_pars = %w[ka kd k]
        when :MichaelisMenten
          check_pars = %w[KM kcat]
        when :reversible
          check_pars = %w[ka kd]
        else
          check_pars = ['k']
        end
        check_pars.each { |par| raise "missing #{par} for #{@type} reaction" unless @parameters.map(&:name).include?(par) }
      end

      # Check that units are consistent
      return unless rxn.check_units
      units = Rxntoarb.options[:none_centred] ? [] : [@rate_units]
      if @type == :reversible || @type == :twostep
        units << "#{rxn.par_units['ka']} #{@reactants.map(&:units_power).join(' ')}" # units of forward reaction rate
        units << "#{rxn.par_units['kd']} #{(@type == :twostep ? @intermediates : @products).map(&:units_power).join(' ')}" # units of reverse reaction rate
      end
      if @type == :irreversible || @type == :twostep
        units << "#{rxn.par_units['k']} #{(@type == :twostep ? @intermediates : @reactants).map(&:units_power).join(' ')}" # units of reaction rate
      end
      if @type == :MichaelisMenten
        rxn.error("units of reactant concentration (#{@reactants.first.units}) inconsistent with units of KM (#{rxn.par_units['KM']})", type=:WARNING) unless @reactants.first.units == rxn.par_units['KM']
        units << "#{rxn.par_units['kcat']} #{@enzyme.first.units}" # units of reaction rate
      end
      units.map! { |unit| Units.convert(unit, '').last }.uniq! # units array will be left with only one element if units are consistent
      rxn.error("inconsistent units in reaction rate (#{units.last} vs #{units.first})", :WARNING) unless units.size == 1 # check that terms in rate expressions have consistent units
      rxn.error("reaction rate has unexpected units", :WARNING) if units.any? { |unit| unit !~ /\Amol m-[23] s-1\z/ } # check that all reaction rates actually have units of reaction rate

    end #}}}

    alias inspect label

  end

end

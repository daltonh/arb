# Rxntoarb::Parameter
# (C) Copyright Christian Biscombe 2016-2017

require_relative '../units'

module Rxntoarb

  class Parameter

    ALIASES = {'k' => 'k', 'ka' => 'ka', 'kf' => 'ka', 'kon' => 'ka', 'kd' => 'kd', 'kr' => 'kd', 'koff' => 'kd', 'KM' => 'KM', 'Km' => 'KM', 'kcat' => 'kcat'}
    attr_reader :name, :units, :value

    def initialize(name, value, units) #{{{
      raise "unknown parameter #{name}" unless ALIASES[name]
      @name = ALIASES[name]
      units = nil if value =~ /^['"]/ # if parameter is defined in terms of a previously defined parameter (i.e. it's a string), then it won't/shouldn't have units
      if units
        value, units = Units.convert("#{value} #{units}", '', {double_precision: true, sig_figs: true}) # convert to SI units
      end
      @value = value
      @units = units
    end #}}}

    def inspect #{{{
      "#{@name} = #{@value}#{" #{@units}" if @units}"
    end #}}}

  end

end

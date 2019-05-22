# Rxntoarb::Parameter
# (C) Copyright Christian Biscombe 2016-2019

require_relative '../units'

module Rxntoarb

  class Parameter

    ALIASES = {'k' => 'k', 'ka' => 'ka', 'kf' => 'ka', 'kon' => 'ka', 'kd' => 'kd', 'kr' => 'kd', 'koff' => 'kd', 'KM' => 'KM', 'Km' => 'KM', 'kcat' => 'kcat'}
    attr_reader :name, :type, :units, :value

    def initialize(name, value, units) #{{{
      @type = (value =~ /^['"]/ ? :LOCAL : :CONSTANT)
      if @type == :LOCAL
        units = nil # parameter is a string, so it won't/shouldn't have units
      else
        value, units = Units.convert("#{value} #{units}", '', {double_precision: true, sig_figs: true}) # convert to SI units
        value = "\"#{value}\"" if name[-1] == '*' # make value an expression constant rather than a numerical constant
      end
      @value = value
      @units = units
      name.chop! if name[-1] == '*'
      raise "unknown parameter #{name}" unless ALIASES[name]
      @name = ALIASES[name]
    ensure
      parameter = self
      Rxntoarb.print_debug(:parameter){}
    end #}}}

    def inspect #{{{
      "#{@name} = #{@value}#{" #{@units}" if @units}"
    end #}}}

  end

end

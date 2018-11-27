# Units module
# (C) Copyright Christian Biscombe 2017-2018

module Units

  VERSION = '1.10'
  DATE = '2018-11-27'

  module_function

  # SI prefixes #{{{
  PREFIXES = {
         'Y' => 1e+24,
         'Z' => 1e+21,
         'E' => 1e+18,
         'P' => 1e+15,
         'T' => 1e+12,
         'G' => 1e+09,
         'M' => 1e+06,
         'k' => 1e+03,
         'h' => 1e+02,
        'da' => 1e+01,
         'd' => 1e-01,
         'c' => 1e-02,
         'm' => 1e-03,
    "\u00b5" => 1e-06, # micro sign
    "\u03bc" => 1e-06, # Greek small letter mu
         'u' => 1e-06, # closest ASCII
         'n' => 1e-09,
         'p' => 1e-12,
         'f' => 1e-15,
         'a' => 1e-18,
         'z' => 1e-21,
         'y' => 1e-24
  }
  PREFIXES.default = 1.0 # default value if no prefix given #}}}

  # Create unit definitions #{{{
  Unit = Struct.new(:name, :sym, :si, :factor, :offset) # attributes are unit name, unit symbol(s) (as array if more than one), equivalent SI units, factor to multiply by to convert to SI units, offset (only for temperatures)
  UNIT_LIST = [
    # base SI units
    Unit.new('ampere', 'A', 'A', 1.0),
    Unit.new('candela', 'cd', 'cd', 1.0),
    Unit.new('kilogram', 'kg', 'kg', 1.0),
    Unit.new('kelvin', 'K', 'K', 1.0, 0.0),
    Unit.new('metre', 'm', 'm', 1.0),
    Unit.new('mole', 'mol', 'mol', 1.0),
    Unit.new('second', 's', 's', 1.0),
    # derived units
    Unit.new('becquerel', 'Bq', 's-1', 1.0),
    Unit.new('coulomb', 'C', 'A s', 1.0),
    Unit.new('degree Celsius', ['degC', "\u2103", "\u00b0C"], 'K', 1.0, 273.15), # special case - could refer to absolute temperature or (with -t flag) temperature difference
    Unit.new('farad', 'F', 'A2 s4 kg-1 m-2', 1.0),
    Unit.new('', 'g', 'kg', 1e-3), # gram, needed so that prefixes other than k will match. Named '' so that it doesn't appear in unit list
    Unit.new('gray', 'Gy', 'm2 s-2', 1.0),
    Unit.new('henry', 'H', 'kg m2 A-2 s-2', 1.0),
    Unit.new('hertz', 'Hz', 's-1', 1.0),
    Unit.new('joule', 'J', 'kg m2 s-2', 1.0),
    Unit.new('katal', 'kat', 'mol s-1', 1.0),
    Unit.new('lumen', 'lm', 'cd', 1.0),
    Unit.new('lux', 'lx', 'cd m-2', 1.0),
    Unit.new('newton', 'N', 'kg m s-2', 1.0),
    Unit.new('ohm', ['ohm', "\u03a9", "\u2126"], 'kg m2 A-2 s-3', 1.0),
    Unit.new('pascal', 'Pa', 'kg m-1 s-2', 1.0),
    Unit.new('radian', 'rad', 'rad', 1.0),
    Unit.new('steradian', 'sr', 'sr', 1.0),
    Unit.new('siemens', 'S', 'A2 s3 kg-1 m-2', 1.0),
    Unit.new('sievert', 'Sv', 'm2 s-2', 1.0),
    Unit.new('tesla', 'T', 'kg A-1 s-2', 1.0),
    Unit.new('volt', 'V', 'kg m2 A-1 s-3', 1.0),
    Unit.new('watt', 'W', 'kg m2 s-3', 1.0),
    Unit.new('weber', 'Wb', 'kg m2 A-1 s-2', 1.0),
    # other units used with the SI
    Unit.new('angstrom', ['angstrom', "\u00c5", "\u212b"], 'm', 1e-10),
    Unit.new('are', 'a', 'm2', 1e2),
    Unit.new('atmosphere (standard)', 'atm', 'kg m-1 s-2', 1.01325e5),
    Unit.new('atmosphere (technical)', 'at', 'kg m-1 s-2', 9.80665e4),
    Unit.new('bar', 'bar', 'kg m-1 s-2', 1e5),
    Unit.new('day', 'd', 's', 8.64e4), # cd (candela) and yd (yard) ambiguous, but centidays and yoctodays aren't cool
    Unit.new('dalton', 'Da', 'kg mol-1', 1e-3),
    Unit.new('electronvolt', 'eV', 'kg m2 s-2', 1.6021766208e-19),
    Unit.new('hour', 'h', 's', 3.6e3),
    Unit.new('litre', ['l', 'L'], 'm3', 1e-3),
    Unit.new('mho', ['mho', "\u2127"], 'A2 s3 kg-1 m-2', 1.0),
    Unit.new('minute', 'min', 's', 60.0),
    Unit.new('molar', 'M', 'mol m-3', 1e3),
    Unit.new('tonne', 't', 'kg', 1e3), # ft (foot) ambiguous, but femtotonnes aren't cool
    Unit.new('unified atomic mass unit', 'u', 'kg mol-1', 1e-3),
    Unit.new('degree (angle)', ['deg', "\u00b0"], 'rad', Math::PI/180.0),
    Unit.new('minute (angle)', ["'", "\u2032"], 'rad', Math::PI/1.08e4),
    Unit.new('second (angle)', ["''", "\u2033", "\u2032\u2032"], 'rad', Math::PI/6.48e5),
    # others
    Unit.new('acre', 'ac', 'm2', 4.0468564224e3),
    Unit.new('British thermal unit', 'BTU', 'kg m2 s-2', 1055.06),
    Unit.new('calorie (thermochemical)', 'cal', 'kg m2 s-2', 4.184),
    Unit.new('degree Fahrenheit', ['degF', "\u2109", "\u00b0F"], 'K', 5.0/9.0, 459.67), # special case - could refer to absolute temperature or (with -t flag) temperature difference
    Unit.new('degree Rankine', ['degR', "\u00b0R"], 'K', 5.0/9.0, 0.0), # special case - could refer to absolute temperature or (with -t flag) temperature difference
    Unit.new('dyne', 'dyn', 'kg m s-2', 1e-5),
    Unit.new('erg', 'erg', 'kg m2 s-2', 1e-7),
    Unit.new('faraday', 'faraday', 'A s', 9.648533289e4),
    Unit.new('foot', 'ft', 'm', 0.3048),
    Unit.new('gallon (UK)', 'gal_UK', 'm3', 4.54609e-3),
    Unit.new('gallon (US)', 'gal_US', 'm3', 3.785411784e-3),
    Unit.new('gauss', 'G', 'kg A-1 s-2', 1e-4),
    Unit.new('horsepower (imperial)', 'hp', 'kg m2 s-3', 745.69987158227022),
    Unit.new('inch', 'in', 'm', 0.0254),
    Unit.new('maxwell', 'Mx', 'kg m2 A-1 s-2', 1e-8),
    Unit.new('mile', 'mile', 'm', 1609.344),
    Unit.new('millimetre of mercury', 'mmHg', 'kg m-1 s-2', 133.322387415),
    Unit.new('ounce (avoirdupois)', 'oz', 'kg', 28.349523125e-3),
    Unit.new('poise', 'P', 'kg m-1 s-1', 0.1),
    Unit.new('pound (avoirdupois)', 'lb', 'kg', 0.45359237),
    Unit.new('pound-force', 'lbf', 'kg m s-2', 4.4482216152605),
    Unit.new('pound per square inch', 'psi', 'kg m-1 s-2', 6.894757e3),
    Unit.new('revolution', ['r', 'rev'], 'rad', 2*Math::PI),
    Unit.new('revolution per minute', 'rpm', 'rad s-1', 2*Math::PI/60),
    Unit.new('torr', 'Torr', 'kg m-1 s-2', 1.01325e5/7.6e2),
    Unit.new('yard', 'yd', 'm', 0.9144),
  ].sort_by { |unit| unit.name.downcase } #}}}

  UNITS = {}
  UNIT_LIST.each do |unit| 
    [unit.sym].flatten.each { |sym| UNITS[sym] = unit }
  end
  
  UNIT_MATCH = /\A(#{Regexp.union(PREFIXES.keys)})??(#{Regexp.union(UNITS.keys)})\^?([-+]?\d+(?:\.\d+)?(?:\/\d+(?:\.\d+)?)?)?\z/

  UnitIO = Struct.new(:value, :units, :dim, :factor)

  def list #{{{
    UNIT_LIST.each { |unit| puts "#{unit.name}: #{[unit.sym].flatten.join(', ')}" unless unit.name.empty? }
  end #}}}

  def convert(input_string, output_string, options={}) #{{{
    input = UnitIO.new
    output = UnitIO.new
    input.value, input.units = /\A\s*([-+]?\d+\.?\d*(?:[DdEe][-+]?\d+)?)?\s*(.*?)\s*\z/.match(input_string).captures
    input.value ||= '1.0'
    options[:double_precision] = true if input.value.downcase.include?('d') # double precision output if double precision input
    input.value = floatify(input.value)
    input.factor, input.dim = convert_SI(input.units)
    output.units = output_string.strip
    if output.units.empty? # default to SI units
      output.factor, output.dim = 1.0, input.dim
      [1, -1].each do |sign| # construct output units string
        Hash[input.dim.sort].each { |unit, dim| output.units << "#{unit}#{(dim.is_a?(Float) ? sprintf('%g', dim) : dim) unless dim == 1} " if sign*dim > 0 }
      end
      output.units.strip!
    else
      output.factor, output.dim = convert_SI(output.units)
    end
    raise "input and output units have different dimensions" unless input.dim == output.dim

    if input.dim == {'K' => 1} && !options[:tdiff] # input refers to an absolute temperature
      _, units, _ = extract(input.units)
      temp_in_K = input.value.to_f*input.factor + UNITS[units].offset*UNITS[units].factor
      _, units, _ = extract(output.units)
      output.value = (temp_in_K - UNITS[units].offset*UNITS[units].factor)/output.factor
    else # any other input, including temperature difference
      output.value = input.factor/output.factor*input.value.to_f
    end

    # Format numerical value
    output.value = if options[:sig_figs]
                     format_sigfigs(output.value, num_sigfigs(input.value))
    elsif options[:format]
                     sprintf(options[:format], output.value)
                   else
                     output.value.to_s
    end

    # Convert to double precision if required
    if options[:double_precision]
      output.value.tr!('Ee', 'd') # express converted value as double precision
      output.value << 'd0' unless output.value.include?('d') # add exponent if not present
      output.value.sub!('d', '.d') unless output.value.include?('.') # add decimal point if not present
    end

    return output.value, output.units.strip
  end #}}}

  def floatify(n) #{{{
    n.tr('DdE', 'e').sub(/\.e/, 'e').sub(/\.\z/, '') # make n a valid Ruby float (stored as a string)
  end #}}}

  def num_sigfigs(n) #{{{
    digits = n.split.first[/(\d|\.)*/].tr('.', '') # strip exponent and decimal point if present
    digits.length-digits.index(/[1-9]/)
  end #}}}

  def format_sigfigs(n, sf) #{{{
    floatify(sprintf('%#.*g', sf, n))
  end #}}}

  def convert_SI(units) #{{{
    factor = 1.0 # this will be the factor to multiply units by to convert to SI
    dim = Hash.new(0) # this will contain the dimensions of units in terms of SI base units
    units.scan(/(.+?)(?:\s+|\s*[.*]\s*(?=[A-z])|\z)/).each do |part|
      prefix, unit, exponent = extract(part.join)
      factor *= (PREFIXES[prefix]*UNITS[unit].factor)**exponent
      # determine the dimensions of each of the SI units comprising units
      UNITS[unit].si.split.each do |part|
        _, si_unit, si_exponent = extract(part)
        si_unit = 'kg' if si_unit == 'g' # kilograms are a special case (base unit contains prefix)
        dim[si_unit] += si_exponent*exponent
      end
    end
    dim.each do |unit, exponent|
      dim[unit] = exponent.to_i if exponent.denominator == 1 # integer exponent
      dim[unit] = exponent.to_f if units[/\d\.\d/] # if any exponent is decimal then make all non-integer exponents decimal
    end
    return factor, dim
  end #}}}

  def extract(part) #{{{
    match = UNIT_MATCH.match(part)
    raise "unrecognised unit #{part}" unless match
    prefix, unit, exponent = match.captures
    exponent ||= '1'
    exponent = if exponent.include?('.') # decimal exponent (possibly with numerator and denominator; division performed by eval below), stored as rational for greatest precision
                 (eval exponent).rationalize
               elsif exponent.include?('/') # rational exponent
                 exponent.to_r
               else # integer exponent
                 exponent.to_i
               end
    return prefix, unit, exponent
  end #}}}

  private_constant :PREFIXES, :UNIT_LIST, :UNIT_MATCH, :UNITS
  private_class_method :convert_SI, :extract

end

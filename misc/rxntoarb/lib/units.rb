# Units module
# (C) Copyright Christian Biscombe 2017

module Units

  module_function

  # SI prefixes
  PREFIXES = {
    'Y' => 1e24,
    'Z' => 1e21,
    'E' => 1e18,
    'P' => 1e15,
    'T' => 1e12,
    'G' => 1e9,
    'M' => 1e6,
    'k' => 1e3,
    'h' => 1e2,
    'da' => 1e1,
    'd' => 1e-1,
    'c' => 1e-2,
    'm' => 1e-3,
    'u' => 1e-6,
    'n' => 1e-9,
    'p' => 1e-12,
    'f' => 1e-15,
    'a' => 1e-18,
    'z' => 1e-21,
    'y' => 1e-24
  }
  PREFIXES.default = 1.0 # default value if no prefix given

  SI_UNITS = %w[A cd kg K m mol rad s sr]

  # Create unit definitions
  Unit = Struct.new(:name, :sym, :si, :factor, :offset) # attributes are unit name, unit symbol, equivalent SI units, factor to multiply by to convert to SI units, offset (only for temperatures)
  UNIT_LIST = [
    # base SI units
    Unit.new('ampere', 'A', 'A', 1.0),
    Unit.new('candela', 'cd', 'cd', 1.0),
    Unit.new('kilogram', 'kg', 'kg', 1.0),
    Unit.new('kelvin', 'K', 'K', 1.0),
    Unit.new('metre', 'm', 'm', 1.0),
    Unit.new('mole', 'mol', 'mol', 1.0),
    Unit.new('second', 's', 's', 1.0),
    # derived units
    Unit.new('becquerel', 'Bq', 's-1', 1.0),
    Unit.new('coulomb', 'C', 'A s', 1.0),
    Unit.new('degree Celsius', 'degC', 'K', 1.0, 273.15), # special case - could refer to absolute temperature or (with -t flag) temperature difference
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
    Unit.new('ohm', 'ohm', 'kg m2 A-2 s-3', 1.0),
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
    Unit.new('angstrom', 'angstrom', 'm', 1e-10),
    Unit.new('atmosphere', 'atm', 'kg m-1 s-2', 1.01325e5),
    Unit.new('bar', 'bar', 'kg m-1 s-2', 1e5),
    Unit.new('day', 'day', 's', 8.64e4), # can't use d because cd and yd would be ambiguous
    Unit.new('dalton', 'Da', 'kg mol-1', 1e-3),
    Unit.new('electronvolt', 'eV', 'kg m2 s-2', 1.6021766208e-19),
    Unit.new('hour', 'h', 's', 3.6e3),
    Unit.new('litre', 'l', 'm3', 1e-3),
    Unit.new('litre', 'L', 'm3', 1e-3),
    Unit.new('minute', 'min', 's', 60.0),
    Unit.new('molar', 'M', 'mol m-3', 1e3),
    Unit.new('tonne', 'tonne', 'kg', 1e3), # can't use t because ft would be ambiguous
    Unit.new('unified atomic mass unit', 'u', 'kg mol-1', 1e-3),
    Unit.new('degree (angle)', 'deg', 'rad', Math::PI/180.0),
    Unit.new('minute (angle)', "'", 'rad', Math::PI/1.08e4),
    Unit.new('second (angle)', "''", 'rad', Math::PI/6.48e5),
    # others
    Unit.new('British thermal unit', 'BTU', 'kg m2 s-2', 1055.06),
    Unit.new('calorie (thermochemical)', 'cal', 'kg m2 s-2', 4.184),
    Unit.new('degree Fahrenheit', 'degF', 'K', 5.0/9.0, 459.67), # special case - could refer to absolute temperature or (with -t flag) temperature difference
    Unit.new('degree Rankine', 'degR', 'K', 5.0/9.0, 0.0), # special case - could refer to absolute temperature or (with -t flag) temperature difference
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
    Unit.new('millimetres of mercury', 'mmHg', 'kg m-1 s-2', 133.322387415),
    Unit.new('ounce (avoirdupois)', 'oz', 'kg', 28.349523125e-3),
    Unit.new('poise', 'P', 'kg m-1 s-1', 0.1),
    Unit.new('pound', 'lb', 'kg', 0.45359237),
    Unit.new('pound-force', 'lbf', 'kg m s-2', 4.4482216152605),
    Unit.new('pounds per square inch', 'psi', 'kg m-1 s-2', 6.894757e3),
    Unit.new('revolutions per minute', 'rpm', 'rad s-1', 2.0*Math::PI/60.0),
    Unit.new('yard', 'yd', 'm', 0.9144),
  ].sort_by { |unit| unit.name.downcase }

  UNITS = {}
  UNIT_LIST.each { |unit| UNITS[unit.sym] = unit }
  
  UNIT_MATCH = /\A(#{Regexp.union(PREFIXES.keys)})??(#{Regexp.union(UNITS.keys)})\^?((?:\+|-)?\d+)?\z/

  # Print list of available units
  def list()
    UNIT_LIST.each { |unit| puts "#{unit.name}: #{unit.sym}" unless unit.name.empty? }
  end

  # Extract prefix, unit, and exponent
  def extract(part)
    match = UNIT_MATCH.match(part)
    raise "unrecognised unit #{part}" unless match
    prefix, unit, exponent = match.captures
    return prefix, unit, exponent ? exponent.to_i : 1
  end

  # Convert to SI units
  def convert_SI(units)
    factor = 1.0 # this will be the factor to multiply units by to convert to SI
    units_dim = {} # this will contain the dimensions of units in terms of SI base units
    SI_UNITS.each { |unit| units_dim[unit] = 0 }
    units.split(/ |\.|\*/).each do |part|
      prefix, unit, exponent = extract(part)
      factor *= (PREFIXES[prefix]*UNITS[unit].factor)**exponent
      # determine the dimensions of each of the SI units comprising units
      UNITS[unit].si.split.each do |part|
        _, si_unit, si_exponent = extract(part)
        si_unit = 'kg' if si_unit == 'g' # kilograms are a special case (base unit contains prefix)
        units_dim[si_unit] += si_exponent*exponent
      end
    end
    return factor, units_dim
  end

  # Convert input units to output units (via conversion to SI)
  def convert(string_in, units_out, options={})
    value_in, units_in = /\A\s*([-+]?\d+\.?\d*(?:[DdEe][-+]?\d+)?)?\s*(.*?)\s*\z/.match(string_in).captures
    value_in ||= '1.0'
    options[:double_precision] = true if value_in.downcase.include?('d') # double precision output if double precision input
    value_in = value_in.tr('DdE', 'e').sub(/\.e/, 'e').sub(/\.\z/, '') # ensure that value_in is a valid Ruby float
    factor_in, units_in_dim = convert_SI(units_in)
    if units_out.empty? # default to SI units
      factor_out, units_out_dim = 1.0, units_in_dim
      [1, -1].each do |sign| # construct output units string
        units_in_dim.each { |unit, dim| units_out << "#{unit}#{dim unless dim == 1} " if sign*dim > 0 }
      end
    else
      factor_out, units_out_dim = convert_SI(units_out.strip)
    end
    raise "input and output units have different dimensions" unless units_in_dim == units_out_dim

    if units_in =~ /\A(deg[CFR]|K)\z/ && !options[:tdiff] # input refers to an absolute temperature
      temp_in_K = units_in =~ /deg[CFR]/ ? (value_in.to_f + UNITS[units_in].offset)*UNITS[units_in].factor : value_in.to_f
      value_out = units_out =~ /deg[CFR]/ ? temp_in_K/UNITS[units_out].factor - UNITS[units_out].offset : temp_in_K
    else
      value_out = factor_in/factor_out*value_in.to_f
    end

    # Format numerical value if required
    if options[:sig_figs]
      digits = value_in[/(\d|\.)*/].tr('.', '') # strip exponent and decimal point if present
      value_out = "#{sprintf('%#.*g', digits.length-digits.index(/[1-9]/), value_out)}".sub(/\.e/, 'e').sub(/\.\z/, '') # subs here ensure that value_out is a valid Ruby float (stored in a string)
    elsif options[:format]
      value_out = sprintf(options[:format], value_out)
    end

    # Convert to double precision format if required
    if options[:double_precision]
      value_out.tr!('Ee', 'd') # express converted value as double precision
      value_out << 'd0' unless value_out.include?('d') # add exponent if not present
      value_out.sub!('d', '.d') unless value_out.include?('.') # add decimal point if not present
    end

    return value_out, units_out.strip
  end

  private_constant :PREFIXES, :SI_UNITS, :UNIT_LIST, :UNIT_MATCH, :UNITS
  private_class_method :extract, :convert_SI

end

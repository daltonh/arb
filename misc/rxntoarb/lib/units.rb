# Unit conversion module
# (C) Copyright Christian Biscombe 2017
# 2017-08-31

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

  # LHS unit is equal to factor on RHS multiplied by base unit on RHS
  # Can be extended by simply adding entries. Order doesn't matter.
  UNITS = {
    # base SI units
    'A' => ['ampere',1.0,'A'],
    'cd' => ['candela',1.0,'cd'],
    'kg' => ['kilogram',1.0,'kg'],
    'K' => ['kelvin',1.0,'K'],
    'm' => ['metre',1.0,'m'],
    'mol' => ['mole',1.0,'mol'],
    's' => ['second',1.0,'s'],
    # derived units
    'Bq' => ['becquerel',1.0,'s-1'],
    'C' => ['coulomb',1.0,'A s'],
    'F' => ['farad',1.0,'A2 s4 kg-1 m-2'],
    'g' => ['',1e-3,'kg'], # needed so that prefixes other than k will match. Named '' so that it doesn't appear in unit list
    'Gy' => ['gray',1.0,'m2 s-2'],
    'H' => ['henry',1.0,'kg m2 A-2 s-2'],
    'Hz' => ['hertz',1.0,'s-1'],
    'J' => ['joule',1.0,'kg m2 s-2'],
    'kat' => ['katal',1.0,'mol s-1'],
    'lm' => ['lumen',1.0,'cd'],
    'lx' => ['lux',1.0,'cd m-2'],
    'N' => ['newton',1.0,'kg m s-2'],
    'ohm' => ['ohm',1.0,'kg m2 A-2 s-3'],
    'Pa' => ['pascal',1.0,'kg m-1 s-2'],
    'rad' => ['radian',1.0,'rad'],
    'sr' => ['steradian',1.0,'sr'],
    'S' => ['siemens',1.0,'A2 s3 kg-1 m-2'],
    'Sv' => ['sievert',1.0,'m2 s-2'],
    'T' => ['tesla',1.0,'kg A-1 s-2'],
    'V' => ['volt',1.0,'kg m2 A-1 s-3'],
    'W' => ['watt',1.0,'kg m2 s-3'],
    'Wb' => ['weber',1.0,'kg m2 A-1 s-2'],
    'degC' => ['degree Celsius',1.0,'K',273.15], # special case - could refer to absolute temperature or (with -t flag) temperature difference
    # other units used with the SI
    'angstrom' => ['angstrom',1e-10,'m'],
    'atm' => ['atmosphere',1.01325e5,'kg m-1 s-2'],
    'bar' => ['bar',1e5,'kg m-1 s-2'],
    'day' => ['day',8.64e4,'s'], # can't use d as cd and yd would be ambiguous
    'Da' => ['dalton',1e-3,'kg mol-1'],
    'eV' => ['electronvolt',1.6021766208e-19,'kg m2 s-2'],
    'h' => ['hour',3.6e3,'s'],
    'l' => ['litre',1e-3,'m3'],
    'L' => ['litre',1e-3,'m3'],
    'min' => ['minute',60.0,'s'],
    'M' => ['molar',1e3,'mol m-3'],
    'tonne' => ['tonne',1e3,'kg'], # can't use t as ft would be ambiguous
    'u' => ['unified atomic mass unit',1e-3,'kg mol-1'],
    'deg' => ['degree (angle)',Math::PI/180.0,'rad'],
    "'" => ['minute (angle)',Math::PI/1.08e4,'rad'],
    "''" => ['second (angle)',Math::PI/6.48e5,'rad'],
    # others
    'BTU' => ['British thermal unit',1055.06,'kg m2 s-2'],
    'dyn' => ['dyne',1e-5,'kg m s-2'],
    'erg' => ['erg',1e-7,'kg m2 s-2'],
    'ft' => ['foot',0.3048,'m'],
    'gal_UK' => ['gallon (UK)',4.54609e-3,'m3'],
    'gal_US' => ['gallon (US)',3.785411784e-3,'m3'],
    'hp' => ['horsepower (imperial)',745.69987158227022,'kg m2 s-3'],
    'in' => ['inch',0.0254,'m'],
    'lb' => ['pound',0.45359237,'kg'],
    'lbf' => ['pound-force',4.4482216152605,'kg m s-2'],
    'mile' => ['mile',1609.344,'m'],
    'mmHg' => ['millimetre of mercury',133.322387415,'kg m-1 s-2'],
    'oz' => ['ounce',28.349523125e-3,'kg'],
    'psi' => ['pounds per square inch',6.894757e3,'kg m-1 s-2'],
    'P' => ['poise',0.1,'kg m-1 s-1'],
    'yd' => ['yard',0.9144,'m'],
    'degF' => ['degree Fahrenheit',5.0/9.0,'K',459.67], # special case - could refer to absolute temperature or (with -t flag) temperature difference
    'degR' => ['degree Rankine',5.0/9.0,'K',0.0], # special case - could refer to absolute temperature or (with -t flag) temperature difference
  }

  # Print list of available units
  def list()
    list = UNITS.sort_by { |_,name| name[0].downcase }
    list.each { |unit| puts "#{unit[1][0]}: #{unit[0]}" unless unit[1][0].empty? }
  end

  # Extract prefix, unit, and exponent
  def extract(part)
    match = /\A(#{Regexp.union(PREFIXES.keys)})??(#{Regexp.union(UNITS.keys)})\^?((?:\+|-)?\d+)?\z/.match(part)
    raise "unrecognised unit #{part}" unless match
    prefix, unit, exponent = match.captures
    return prefix, unit, exponent ? exponent.to_i : 1
  end

  # Convert to SI units
  def convert_SI(units)
    factor = 1.0 # this will be the factor to multiply units by to convert to SI
    units_dim = Array.new(SI_UNITS.length,0) # this will contain the dimensions of units ordered the same way as SI_UNITS
    units.split(/ |\.|\*/).each do |part|
      prefix, unit, exponent = extract(part)
      factor *= (PREFIXES[prefix]*UNITS[unit][1])**exponent
      # determine the dimensions of each input unit (ordered the same way as SI_UNITS)
      dim = Array.new(SI_UNITS.length,0)
      UNITS[unit][2].split(/ |\.|\*/).each do |part|
        _, si_unit, si_exponent = extract(part)
        si_unit = 'kg' if si_unit == 'g' # kilograms are a special case (base unit contains prefix)
        dim[SI_UNITS.index(si_unit)] = si_exponent
      end
      units_dim = units_dim.map.with_index { |d,i| d+dim[i]*exponent }
    end
    return factor, units_dim
  end

  # Convert input units to output units (via conversion to SI)
  def convert(string_in,units_out,sig_figs=false,tdiff=false)
    value_in, units_in = /\A\s*([-+]?\d+\.?\d*(?:[DdEe][-+]?\d+)?)?\s*(.*?)\s*\z/.match(string_in).captures
    value_in ||= '1.0'
    value_in = value_in.tr('DdE','e').sub(/\.e/,'e').sub(/\.\z/,'') # ensure that value_in is a valid Ruby float
    factor_in, units_in_dim = convert_SI(units_in)
    if units_out.empty? # default to SI units
      factor_out, units_out_dim = 1.0, units_in_dim
      [1,-1].each do |sign| # construct output units string
        units_in_dim.each.with_index { |d,i| units_out << "#{SI_UNITS[i]}#{d unless d == 1} " if sign*d > 0 }
      end
    else
      factor_out, units_out_dim = convert_SI(units_out.strip)
    end
    abort "ERROR: input and output units have different dimensions" unless units_in_dim == units_out_dim

    if units_in =~ /\A(deg[CFR]|K)\z/ && !tdiff # assume input refers to an absolute temperature
      temp_in_K = units_in =~ /deg[CFR]/ ? (value_in.to_f + UNITS[units_in][3])*UNITS[units_in][1] : value_in.to_f
      value_out = units_out =~ /deg[CFR]/ ? temp_in_K/UNITS[units_out][1] - UNITS[units_out][3] : temp_in_K
    else
      value_out = factor_in/factor_out*value_in.to_f
    end

    if sig_figs
      digits = value_in[/(\d|\.)*/].tr('.','') # strip exponent and decimal point if present
      value_out = "#{sprintf('%#.*g',digits.length-digits.index(/[1-9]/),value_out)}".sub(/\.e/,'e').sub(/\.\z/,'') # subs here ensure that value_out is a valid Ruby float (stored in a string)
    end

    return value_out, units_out.strip
  end

  private_constant :PREFIXES, :SI_UNITS, :UNITS
  private_class_method :extract, :convert_SI

end

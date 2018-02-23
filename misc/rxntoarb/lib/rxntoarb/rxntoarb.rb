# Rxntoarb
# (C) Copyright Christian Biscombe 2016-2018

require 'optparse'
require_relative 'arb'
require_relative 'rxn'

module Rxntoarb

  PROGNAME = 'rxntoarb'
  VERSION = '2.12'
  DATE = '2018-02-23'
  INFO = <<-INFO.gsub(/^\s+/, '') # prefer squiggly heredoc <<~ in Ruby 2.3+
    #{PROGNAME} v. #{VERSION} (#{DATE})
    Converts a human-readable system of chemical reactions into a set of equations for use with arb finite volume solver.
    (C) Copyright Christian Biscombe 2016-2018
  INFO

  TEMPLATE_NAME = "#{PROGNAME}rc" # rxntoarbrc by default
  TEMPLATE_LOCAL = "#{Dir.pwd}/#{TEMPLATE_NAME}"
  TEMPLATE_GLOBAL = "#{File.dirname(File.realpath(__FILE__))}/../../#{TEMPLATE_NAME}" # prefer __dir__ in Ruby 2.0+

  class << self
    attr_accessor :options
  end

  OPTIONS = OptionParser.new do |opt|
    opt.banner = "#{INFO}\nUsage: #{PROGNAME} [options] <list of input files>, where options may be:"
    opt.on('-d', '--debug', 'Print debugging output') { Rxntoarb.options[:debug] = true }
    opt.on('-i', '--interactive', 'Prompt before overwriting existing output file') { Rxntoarb.options[:interactive] = true }
    opt.on('-n', '--none-centred', 'Activate ODE mode: generate ODEs (no spatial dependence) rather than PDEs') { Rxntoarb.options[:none_centred] = :flag }
    opt.on('-o', '--outfile <output_file>', 'Write output to output_file (option ignored if multiple input files)') { |outfile| Rxntoarb.options[:outfile] = outfile }
  # opt.on('-s', '--sbml <sbml_output_file>', String, 'Write SBML file') { |sbmlfile| require 'libSBML'; Rxntoarb.options[:sbmlfile] = sbmlfile } # TODO in version 3
    opt.on('-t', '--template <template_file>', 'Read arb equation format from template_file') { |template_file| Rxntoarb.options[:template_file] = template_file }
    opt.on('-v', '--version', 'Print version information') { puts INFO; exit if ARGV.empty? }
  end

  module_function

  def run #{{{

    Rxntoarb.options = {}
    Rxntoarb.options[:none_centred] = true # true by default, becomes false when a surface|volume_region statement is encountered in infile; :flag if explicitly in ODE mode (-n flag)
    Rxntoarb.options[:template_file] = File.file?(TEMPLATE_LOCAL) ? TEMPLATE_LOCAL : TEMPLATE_GLOBAL # local template file preferred if it exists, otherwise fall back to global one
    optparse(ARGV) # process command-line options
    if ARGV.empty?
      warn OPTIONS
      abort 'ERROR: no input file specified'
    end

    ARGV.each do |infile|
      abort "ERROR: input file #{infile} does not exist (or isn't a regular file)" unless File.file?(infile)
      warn "WARNING: input file #{infile} has unexpected extension. Is it a valid input file?" unless File.extname(infile) == '.rxn'
      begin
        rxn = Rxn.new(infile)
        rxn.parse
      ensure
        print_debug(:rxn){}
        warn '*'*200 if Rxntoarb.options[:debug]
      end

      outfile = Rxntoarb.options[:outfile] && ARGV.size == 1 ? Rxntoarb.options[:outfile] : "#{File.basename(infile, '.*')}.arb"
      while Rxntoarb.options[:interactive] && File.file?(outfile)
        print "WARNING: file #{outfile} already exists. Overwrite it? [y/n] "
        break if $stdin.gets =~ /^y(es)?$/i
        print "Enter new file name: "
        outfile = $stdin.gets.strip
      end
      begin
        arb = Arb.new(outfile)
        arb.write(rxn)
      ensure
        print_debug(:arb){}
        warn '*'*200 if Rxntoarb.options[:debug]
      end
    end

  end #}}}

  def optparse(argv) #{{{
    OPTIONS.parse!(argv)
    abort "ERROR: template file #{Rxntoarb.options[:template_file]} does not exist (or isn't a regular file)" unless File.file?(Rxntoarb.options[:template_file])
    Rxntoarb.options[:template] = File.read(Rxntoarb.options[:template_file]) # store contents of template_file as a string
  rescue OptionParser::InvalidOption, OptionParser::MissingArgument => msg
    warn OPTIONS
    abort "ERROR: #{msg}"
  ensure
    print_debug(:'Rxntoarb.options'){}
  end #}}}

  def print_debug(vars, &b) #{{{
    return unless Rxntoarb.options[:debug]
    [*vars].each do |var|
      obj = eval(var.to_s, b.binding)
      if obj.instance_variables.empty?
        warn "#{var}: #{obj}"
      else
        obj.instance_variables.each { |ivar| warn "#{var}.#{ivar.to_s.tr('@', '')}: #{obj.instance_variable_get(ivar).inspect}" }
      end
    end
  end #}}}

end

# Rxntoarb
# (C) Copyright Christian Biscombe 2016-2017

require 'optparse'
require_relative 'arb'
require_relative 'rxn'

module Rxntoarb

  PROGNAME = 'rxntoarb'
  VERSION = 2.7
  DATE = '2017-10-26'
  INFO = <<-INFO.gsub(/^\s+/, '') # prefer squiggly heredoc <<~ in Ruby 2.3+
    #{PROGNAME} v. #{VERSION} (#{DATE})
    Converts a human-readable system of chemical reactions into a set of equations for use with arb finite volume solver.
    (C) Copyright Christian Biscombe 2016-2017
  INFO

  class << self
    attr_accessor :options
  end

  module_function

  def run #{{{
    selfdir = File.dirname(File.realpath(__FILE__)) # prefer __dir__ in Ruby 2.0+
    template_name = "#{Rxntoarb::PROGNAME}rc" # rxntoarbrc by default
    template_local = "#{Dir.pwd}/#{template_name}"
    template_global = "#{selfdir}/../../#{template_name}"

    # Process command-line options
    options = {}
    options[:none_centred] = true # none_centred by default; overridden by -n flag or surface|volume_region statements
    options[:template_file] = File.file?(template_local) ? template_local : template_global
    opts = OptionParser.new do |opt|
      opt.banner = "#{Rxntoarb::INFO}\nUsage: #{Rxntoarb::PROGNAME} [options] <list of input files>, where options may be:"
      opt.on('-d', '--debug', 'Print debugging output') { options[:debug] = true }
      opt.on('-i', '--interactive', 'Prompt before overwriting existing output file') { options[:interactive] = true }
      opt.on('-n', '--none-centred', 'Activate ODE mode: generate ODEs (no spatial dependence) rather than PDEs') { options[:none_centred] = :flag }
      opt.on('-o', '--outfile <output_file>', 'Write output to output_file (option ignored if multiple input files)') { |outfile| options[:outfile] = outfile }
    # opt.on('-s', '--sbml <sbml_output_file>', 'Write SBML file') { |sbmlfile| require 'libSBML'; options[:sbmlfile] = sbmlfile } # TODO in version 3
      opt.on('-t', '--template <template_file>', 'Read arb equation format from template_file') { |template_file| options[:template_file] = template_file }
      opt.on('-v', '--version', 'Print version information') { puts info; exit if ARGV.empty? }
    end
    begin opts.parse!
    rescue OptionParser::InvalidOption => msg
      warn opts
      abort "ERROR: #{msg}"
    end
    if ARGV.empty?
      warn opts
      abort 'ERROR: no input file specified'
    end

    abort "ERROR: could not find template file #{options[:template_file]} (or it isn't a regular file)" unless File.file?(options[:template_file])
    options[:template] = File.read(options[:template_file]) # store contents of template_file as a string

    ARGV.each do |infile|
      abort "ERROR: specified input file #{infile} does not exist (or isn't a regular file)" unless File.file?(infile)
      outfile = options[:outfile] && ARGV.size == 1 ? options[:outfile] : "#{File.basename(infile, '.*')}.arb"
      while options[:interactive] && File.file?(outfile)
        print "WARNING: file #{outfile} already exists. Overwrite it? [y/n] "
        break if $stdin.gets =~ /^y(es)?$/i
        print "Enter new file name: "
        outfile = $stdin.gets.strip
      end
      Rxntoarb.options = options
      print_debug(:options){}
      begin
        rxn = Rxn.new(infile)
        rxn.parse
      ensure
        print_debug(:rxn){}
        warn '*'*200 if Rxntoarb.options[:debug]
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

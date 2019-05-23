# Rxntoarb
# (C) Copyright Christian Biscombe 2016-2019

require 'optparse'
require_relative 'arb'
require_relative 'rxn'

module Rxntoarb

  PROGNAME = 'rxntoarb'
  VERSION = '2.26'
  DATE = '2019-05-23'
  INFO = <<-INFO.gsub(/^\s+/, '') # prefer squiggly heredoc <<~ in Ruby 2.3+
    #{PROGNAME} v. #{VERSION} (#{DATE})
    Converts a human-readable system of chemical reactions into a set of equations for use with arb finite volume solver.
    (C) Copyright Christian Biscombe 2016-2019
  INFO

  TEMPLATE_NAME = "#{PROGNAME}rc" # rxntoarbrc by default
  TEMPLATE_LOCAL = "#{Dir.pwd}/#{TEMPLATE_NAME}"
  TEMPLATE_GLOBAL = "#{File.dirname(File.realpath(__FILE__))}/../../#{TEMPLATE_NAME}" # prefer __dir__ in Ruby 2.0+

  class << self
    attr_accessor :options
  end

  OPTIONS = OptionParser.new do |opt|
    opt.banner = "#{INFO}\nUsage: #{PROGNAME} [options] <list of input files>, where options may be:"
    opt.on('-d', '--debug', 'Print debugging output') { self.options[:debug] = true }
    opt.on('-i', '--interactive', 'Prompt before overwriting existing output file') { self.options[:interactive] = true }
    opt.on('-l', '--alias-labels', 'Use aliases as reaction labels') { self.options[:alias_labels] = true }
    opt.on('-n', '--none-centred', 'Activate ODE mode: generate ODEs (no spatial dependence) rather than PDEs') { self.options[:none_centred] = :flag }
    opt.on('-o', '--outfile <output_file>', 'Write output to output_file (option ignored if multiple input files)') { |outfile| self.options[:outfile] = outfile }
    opt.on('-s', '--strict', 'Regard warnings as errors') { self.options[:strict] = true }
    opt.on('-t', '--template <template_file>', 'Read arb equation format from template_file') { |template_file| self.options[:template_file] = template_file }
    opt.on('-v', '--version', 'Print version information') { puts INFO; exit if ARGV.empty? }
  end

  module_function

  def run #{{{

    self.options = {}
    self.options[:none_centred] = true # true by default, becomes false when a surface|volume_region statement is encountered in infile; :flag if explicitly in ODE mode (-n flag)
    self.options[:template_file] = File.file?(TEMPLATE_LOCAL) ? TEMPLATE_LOCAL : TEMPLATE_GLOBAL # local template file preferred if it exists, otherwise fall back to global one
    optparse(ARGV) # process command-line options
    if ARGV.empty?
      warn OPTIONS
      abort 'ERROR: no input file specified'
    end

    ARGV.each do |infile|
      abort "ERROR: input file #{infile} does not exist (or isn't a regular file)" unless File.file?(infile)
      abort "ERROR: input file #{infile} is not readable" unless File.readable?(infile)
      warn "WARNING: input file #{infile} has unexpected extension. Is it a valid input file?" unless File.extname(infile) == '.rxn'
      begin
        rxn = Rxn.new(infile)
        rxn.parse
      ensure
        print_debug(:rxn){}
        warn '*'*200 if self.options[:debug]
      end

      outfile = self.options[:outfile] && ARGV.size == 1 ? self.options[:outfile] : "#{File.basename(infile, '.*')}.arb"
      while self.options[:interactive] && File.file?(outfile)
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
        warn '*'*200 if self.options[:debug]
      end
    end

  end #}}}

  def optparse(argv) #{{{
    OPTIONS.parse!(argv)
    template = self.options[:template_file]
    abort "ERROR: template file #{template} does not exist (or isn't a regular file)" unless File.file?(template)
    abort "ERROR: template file #{template} is not readable" unless File.readable?(template)
    self.options[:template] = File.read(template) # store contents of template_file as a string
  rescue OptionParser::InvalidOption, OptionParser::MissingArgument => msg
    warn OPTIONS
    abort "ERROR: #{msg}"
  ensure
    rxntoarb = self
    print_debug(:'rxntoarb.options'){}
  end #}}}

  def print_debug(*vars, &b) #{{{
    return unless self.options[:debug]
    vars.each do |var|
      obj = eval(var.to_s, b.binding)
      if obj.instance_variables.empty?
        warn "#{var}: #{obj}"
      else
        obj.instance_variables.each { |ivar| warn "#{var}.#{ivar.to_s.tr('@', '')}: #{obj.instance_variable_get(ivar).inspect}" }
      end
    end
  end #}}}

  def debracket(name) #{{{
    name.tr('<>', '') # remove angle brackets from names
  end #}}}

  def arrayify(list) #{{{
    list.split(/,\s*/).map { |region| self.debracket(region).strip } # convert comma-separated list (String) of region names to Array
  end #}}}

end

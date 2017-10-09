# Rxntoarb::Arb
# (C) Copyright Christian Biscombe 2016-2017
# 2017-10-09

require_relative 'parameter'
require_relative 'rxn'

module Rxntoarb

  class Arb

    attr_accessor :aliases, :constants, :equations, :file, :header, :magnitudes, :output, :rates, :sources, :species_present

    def initialize(file) #{{{
      @aliases = {}
      @constants = []
      @equations = []
      @file = file
      @header = []
      @magnitudes = []
      @output = []
      @rates = []
      @sources = {} # key is [species, source_region]
      @species_present = [] # keeps track of the order in which new species appear (used to determine magnitudes)
    end #}}}

    def create_rates(reaction, rxn) #{{{

      @aliases[reaction.aka] ||= reaction.parent_label if reaction.aka
      reaction.all_species.each do |species|
        @sources[[species, Rxntoarb.options[:none_centred] ? nil : species.region]] ||= '0.d0' # ensure that species has a source term originating in its own region
      end

      # Format kinetic parameters
      if reaction.parameters
        reaction.parameters.each do |par|
          @constants << "CONSTANT <#{par.name}_#{reaction.parent_label}>#{" [#{par.units}]" if par.units} #{par.value} #{reaction.comment}#{" # alias: #{reaction.aka} => #{@aliases[reaction.aka]}" if reaction.aka}"
        end
      end

      # Generate rate expressions
      conc_powers = ->(species) { "*<#{species.conc}_pos>#{"**#{species.coeff}" if species.coeff > 1}" }
      if reaction.type == :reversible || reaction.type == :twostep # reversible must come before irreversible because same code handles two-step reactions
        reaction.label << '_i' if reaction.type == :twostep
        @rates << "#{reaction.centring}_DERIVED <R_#{reaction.label}> \"<ka_#{reaction.parent_label}>"
        reaction.reactants.each { |reactant| @rates.last << conc_powers.call(reactant) }
        @rates.last << "-<kd_#{reaction.parent_label}>"
        if reaction.type == :twostep
          reaction.intermediates.each { |intermediate| @rates.last << conc_powers.call(intermediate) }
        else
          reaction.products.each { |product| @rates.last << conc_powers.call(product) }
        end
        create_sources(reaction)
      end
      if reaction.type == :irreversible || reaction.type == :twostep
        reaction.label << 'i' if reaction.type == :twostep
        @rates << "#{reaction.centring}_DERIVED <R_#{reaction.label}> \"<k_#{reaction.parent_label}>"
        if reaction.type == :twostep
          reaction.intermediates.each { |intermediate| @rates.last << conc_powers.call(intermediate) }
        else
          reaction.reactants.each { |reactant| @rates.last << conc_powers.call(reactant) }
        end
        create_sources(reaction)
      end
      if reaction.type == :MichaelisMenten
        reactant_conc = "#{reaction.reactants.first.conc}_pos"
        enzyme_conc = "#{reaction.enzyme.first.conc}_pos"
        @rates << "#{reaction.centring}_DERIVED <R_#{reaction.label}> \"<kcat_#{reaction.parent_label}>*<#{enzyme_conc}>*<#{reactant_conc}>/(<KM_#{reaction.parent_label}>+<#{reactant_conc}>)"
        create_sources(reaction)
      end

    end #}}}

    def write(rxn) #{{{

      # Perform alias substitution on any kinetic parameters defined as strings
      @constants.each do |constant|
        @aliases.each { |old, new| constant.sub!(/<#{$1}_#{$2}>/, "<#{$1}_#{new}>") << " # alias: #{old} => #{new}" if constant =~ /<(#{Regexp.union(Parameter::NAMES.values)})_(#{Regexp.escape(old)})>/ }
      end

      # Store region areas/volumes and define new regions for equations
      unless Rxntoarb.options[:none_centred]
        rxn.surface_regions.each { |region| @constants << "CONSTANT <area(#{region})> \"facesum(<<radius_f>>*<facearea>, region=<#{region}>)\"" }
        rxn.volume_regions.each do |region|
          @constants << "CONSTANT <volume(#{region})> \"cellsum(<<radius_c>>*<cellvol>, region=<#{region}>)\""
          @constants << "FACE_REGION <associatedfaces(#{region})> \"associatedwith(<#{region}>)\""
          @constants << "CELL_REGION <associatedcells(#{region})> \"associatedwith(<#{region}>)\""
          @constants << "CELL_REGION <domainof(#{region})> \"domainof(<#{region}>)\""
          rxn.bounding_regions[region].each do |bounding_region|
            rxn.volume_species[region].each { |species| @sources[[species, bounding_region]] ||= "0.d0" } # ensure that each species within volume_region has a source term on each bounding surface_region
          end
        end
      end

      # Format source terms and generate equations based on template file
      warn "#{'*'*200}\nINFO: creating equations" if Rxntoarb.options[:debug]
      @sources.each do |key, source|
        species, source_region = key
        source_centring = if Rxntoarb.options[:none_centred] # source_centring is the centring of the source_region in which the reaction is occurring
                            :NONE
                          elsif rxn.surface_regions.include?(source_region)
                            :FACE
                          else
                            :CELL
                          end
        if Rxntoarb.options[:none_centred] && rxn.initial_species.include?(species.tag) && source == '0.d0' # species present initially isn't produced or consumed in any reactions (i.e. enzyme only)
          @constants << "CONSTANT <#{species.conc}> \"<#{species.conc}_0>\""
          @constants << "CONSTANT <#{species.conc}_pos> \"<#{species.conc}_0>\""
          @sources[key] = ''
          next
        end
        @sources[key] = "#{source_centring}_#{source == '0.d0' ? 'CONSTANT' : 'DERIVED'} <S_#{species.tag}#{"@#{source_region}" unless Rxntoarb.options[:none_centred]}> \"#{source}\"#{" ON <#{source_region}>" unless Rxntoarb.options[:none_centred]}"
        create_equations(species, source_region, rxn) if species.region == source_region || Rxntoarb.options[:none_centred] # only do equations once for each species
      end
      warn "INFO: equation creation complete" if Rxntoarb.options[:debug]

      # Determine magnitude dependencies for each species by ordering reactions
      # Initially only reactions involving initial species (specified by the initial_species statement) can proceed.
      # Products of these initial reactions are added to species_present.
      # Any reactions that depend on these new products and any of the other previously existing species may now proceed.
      # The process is repeated until all (or as many as possible) of the species in the system have been produced.
      # The magnitude of each species is the minimum of the magnitudes of the reactants that produce it earliest.
      warn "INFO: determining magnitudes" if Rxntoarb.options[:debug]
      size = 0
      rxn.species.each { |species| @species_present << species if rxn.initial_species.include?(species.tag) }
      until size == @species_present.size
        size = @species_present.size
        present = []
        rxn.reactions.each do |reaction|
          next if reaction.all_species.all? { |species| @species_present.include?(species) } # all species in reaction already exist
          next if reaction.type == :MichaelisMenten && !@species_present.include?(reaction.enzyme.first) # enzyme doesn't already exist
          if reaction.type == :twostep
            find_precursors(reaction.reactants, reaction.intermediates, present)
            find_precursors(reaction.intermediates, reaction.reactants, present)
          end
          find_precursors(reaction.reactants, reaction.products, present)
          find_precursors(reaction.products, reaction.reactants, present) if reaction.type == :reversible
        end
        @species_present |= present
      end
      (rxn.species-@species_present).each { |species| warn "WARNING in #{rxn.file}: unable to determine magnitude for species #{species.tag}" }

      # Format magnitudes
      @species_present.each do |species|
        if rxn.initial_species.include?(species.tag)
          species.magnitude = "<#{species.conc}_0>" # magnitudes of initial species are their initial concentrations
        else
          species.precursors.each do |precursor|
            species.magnitude << if species.bound?
                                   if precursor.bound? || Rxntoarb.options[:none_centred]
                                     "nonemin(<#{precursor.conc} magnitude>,"
                                   else
                                     "nonemin(<#{precursor.conc} magnitude>*<volume(#{precursor.region})>/<area(#{species.region})>," # convert surface concentration to volume concentration
                                   end
                                 else
                                   if precursor.free? || Rxntoarb.options[:none_centred]
                                     "nonemin(<#{precursor.conc} magnitude>,"
                                   else
                                     "nonemin(<#{precursor.conc} magnitude>*<area(#{precursor.region})>/<volume(#{species.region})>," # convert volume concentration to surface concentration
                                   end
                                 end
          end
          species.magnitude << "<huge>#{')'*species.precursors.size}"
        end
        @magnitudes << "CONSTANT <#{species.conc} magnitude> \"#{species.magnitude}\""
      end
      warn "INFO: magnitudes done" if Rxntoarb.options[:debug]

      # Format output
      format_output(@header << "# Generated from #{rxn.file} by #{PROGNAME} v. #{VERSION}, #{Time.now.strftime('%F %T')}")
      format_output(@constants, {name: 'Constants and regions'})
      format_output(@rates, {name: 'Reaction rates', pre: 'DEFAULT_OPTIONS output', post: 'DEFAULT_OPTIONS'})
      format_output(@sources.values, {name: 'Source terms', pre: 'DEFAULT_OPTIONS output', post: 'DEFAULT_OPTIONS'})
      format_output(@equations, {name: 'Equations'})
      format_output(@magnitudes, {name: 'Magnitudes'})
      File.write(@file, @output.join("\n"))
      warn "INFO: output written to #{@file}\n#{'*'*200}" if Rxntoarb.options[:debug]

    rescue => msg
      warn "ERROR: #{msg}"
      warn msg.backtrace if Rxntoarb.options[:debug]
      abort
    end #}}}

    private

    def create_sources(reaction) #{{{
      if reaction.type == :twostep
        reactants, products = if reaction.label =~ /_i\z/ # first step
                                [reaction.reactants, reaction.intermediates]
                              else # second step
                                [reaction.intermediates, reaction.products]
                              end
      else
        reactants = reaction.reactants
        products = reaction.products
      end
      [reactants, products].compact.each do |species_array|
        source_sign = species_array == reactants ? '-' : '+'
        species_array.each do |species|
          key = [species, reaction.region]
          @sources[key] = '' if @sources[key] == '0.d0'
          (@sources[key] ||= '') << "#{source_sign}#{"#{species.coeff}.d0*" if species.coeff > 1}<R_#{reaction.label}>" # add rate to source term for this species
        end
      end
      @rates.last << "\"#{" ON <#{reaction.region}>" if reaction.region}" # finalise rate expression
    end #}}}

    def create_equations(species, source_region, rxn) #{{{

      template = Rxntoarb.options[:template].dup

      # Handle if_rxn statements in the template
      # These take the form if_rxn(if_location[=if_region]){if_clause}[{else_clause}], where if_location = none|surface|volume
      # Clauses may span multiple lines. Nested {} pairs (but not nested if_rxn statements) are allowed
      Rxntoarb.options[:template].scan(/^[^#\n]*\bif_rxn\s*(\((?<if_location>\w+)(\s*=\s*(?<if_region>.*?))?\))?\s*(?<if_clause>{((?>[^{}]+)|\g<if_clause>)*})?(?<else_clause>{((?>[^{}]+)|\g<else_clause>)*})?/i) do |if_location, if_region, if_clause, else_clause|
        replace = $&[/\bif_rxn.*/m]
        if_location.downcase!
        raise "missing or unrecognised location in if_rxn statement in template file #{Rxntoarb.options[:template_file]}" unless if_location =~ /\A(none|surface|volume)\z/
        if_region &&= if_region.split(/,\s*/).map { |region| region.tr('<>', '').strip } # convert to array
        if_clause &&= if_clause[1..-2].split("\n").map(&:strip) - [''] # remove braces, strip newlines and indentation, and delete any empty lines
        raise "missing clause in if_rxn statement in template file #{Rxntoarb.options[:template_file]} (possibly missing opening or closing brace)" unless if_clause
        else_clause &&= else_clause[1..-2].split("\n").map(&:strip) - [''] # remove braces, strip newlines and indentation, and delete any empty lines
        condition = if_location == species.location.to_s # condition will be true if we are in the right location (species is located in same region as source here)
        condition = condition && if_region.include?(source_region) if if_region # if an if_region has been specified, condition will be true if we are in the right region
        template.sub!(replace, condition ? if_clause.join("\n") : else_clause.to_a.join("\n")) # replace if_rxn statement with if_clause if condition is true, otherwise replace with else_clause (if present) or empty string
      end

      # Handle each blocks in the template
      # These take the form [region_list].each { |loopvar| expression }
      # '*' in region_list is a shorthand for all reaction regions (surface_regions that bound volume_region)
      if rxn.volume_regions.include?(source_region)
        template.dup.scan(/^\s*(?<array>\[((?>[^\[\]]+)|\g<array>)*\])\.each\s*(?<brace_group>{((?>[^{}]+)|\g<brace_group>)*})?/i) do |array, block|
          replace = $&[/^\s*(.*)/m, 1]
          region_list = array[1..-2].split(/,\s*/).map { |region| region.tr('<>', '').strip } # convert to array
          if region_list.include?('*') # add all reaction regions to region_list
            region_list[region_list.index('*')] = rxn.bounding_regions[source_region]
            region_list.flatten!.uniq!
          end
          raise "missing each block in template file #{Rxntoarb.options[:template_file]} (possibly missing opening or closing brace)" unless block
          loopvar, expression = block.match(/\A{\s*(\|[^|]+\|)?\s*(.*)}\z/m).captures
          raise "missing loop variable in each block in template file #{Rxntoarb.options[:template_file]}" unless loopvar
          expression = expression.split("\n").map(&:strip) - [''] # strip newlines and indentation and delete any empty lines
          replacement = []
          region_list.each do |region| # duplicate expression for each region in region_list, replacing loopvar with region
            replacement += expression.map { |line| line.gsub(loopvar, region) }
          end
          template.sub!(replace, replacement.join("\n")) # replace the each block
        end
      end

      # Do substitutions on template
      template.gsub!("\n\n", "\n")
      template.gsub!('/c/', species.conc)
      template.gsub!('/species/', species.name)
      template.gsub!('@/region/', species.region ? "@#{species.region}" : '')
      template.gsub!('/region/', species.region ? species.region : '')
      template.gsub!('@/source_region/', Rxntoarb.options[:none_centred] ? '' : "@#{source_region}")
      template.gsub!('/CENTRING/', species.centring)
      template.gsub!('/centring/', species.centring.downcase)
      template.gsub!('/units/', Rxntoarb.options[:none_centred] ? '' : species.units)
      if Rxntoarb.options[:none_centred]
        template.gsub!(/ ON <[^>]+>/, '') # remove references to regions
      else
        template.gsub!('/associatedfaces(region)/', species.location == :surface ? species.region : "associatedfaces(#{species.region})")
        template.gsub!('/associatedcells(region)/', species.location == :surface ? species.region : "associatedcells(#{species.region})")
        template.gsub!('/domainof(region)/', species.location == :surface ? species.region : "domainof(#{species.region})")
      end
      template.gsub!('/MW/', species.mw) if species.location == :volume

      @equations << ["# #{species.tag} {{{", '', template, '#}}}']

    end #}}}

    def find_precursors(reactants, products, present) #{{{
      return unless reactants.all? { |reactant| @species_present.include?(reactant) } # check whether all reactants are present yet; if so add products to the list of species
      products.each do |product|
        next if reactants.include?(product)
        present << product
        product.precursors ||= reactants - products
      end
    end #}}}

    def format_output(array, options={}) #{{{
      array -= ['', []] # remove empty entries
      return if array.empty?
      @output << ["# #{options[:name]} {{{", ''] if options[:name]
      @output << options[:pre] if options[:pre]
      @output << array
      @output << options[:post] if options[:post]
      @output << ''
      @output << '#}}}' if options[:name]
    end #}}}

  end

end

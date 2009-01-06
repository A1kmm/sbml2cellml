#!/usr/bin/env ruby

require 'rexml/document'

class SBMLError < Exception
  def initialize(why)
    @why = why
  end

  def why
    @why
  end
end

class SBMLToCellML
  CELLML_NS = "http://www.cellml.org/cellml/1.1#"
  MATHML_NS = "http://www.w3.org/1998/Math/MathML"

  def self.convert(sbml)
    self.new(sbml).cellml
  end

  def initialize(sbml)
    @sbml = sbml
    create_cellml
    translate_sbml
  end

  attr_reader :sbml, :cellml

  private
  def create_cellml
    @cellml = REXML::Document.new()
    @cellmlEl = @cellml.add_element('model')
    @cellmlEl.add_namespace(CELLML_NS)
  end

  def translate_sbml
    @sbmlEl = @sbml.root
    @level = @sbmlEl.attribute('level').value.to_i
    raise SBMLError.new('Unsupported SBML level %u' % level) unless @level == 2
    @version = @sbmlEl.attribute('version').value.to_i

    @sbml_ns = 'http://www.sbml.org/sbml/level%u' % @level
    @sbml_ns += '/version%u' % @version if @version > 1
    
    raise SBMLError.new('Unexpected namespace %s' % @sbmlEl.namespace) unless @sbmlEl.namespace == @sbml_ns
    raise SBMLError.new('Unexpected element %s' % @sbmlEl.name) unless @sbmlEl.name == 'sbml'

    process_units
    process_functions
    process_parameters
    process_compartments
    process_species
  end

  def process_units
    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfUnitDefinitions/unitDefinition') { |el|
      units = @cellmlEl.add_element('units', {'name' => el.attribute('id').value})
      REXML::XPath.each(el, 'listOfUnits/unit') { |u|
        attrs = {'units' => u.attribute('kind').value }
        {'exponent' => 'exponent', 'scale' => 'prefix', 'multiplier' => 'multiplier'}.each_pair { |sbml,cellml|
          attrs[cellml] = u.attribute(sbml).value unless u.attribute(sbml).nil?
        }
        units.add_element('unit', attrs)
      }
    }
  end

  def ensure_builtin_unit(name)
    return unless REXML::XPath.first(@cellmlEl, "/model/units[@name=\"#{name}\"]").nil?

    units = @cellmlEl.add_element('units', {'name' => name})

    spl = name.split('_per_')
    if (spl.length > 1)
      partno = 0
      spl.each { |part|
        partno = partno + 1
        ensure_builtin_unit(part)
        attrs = {'units' => part}
        attrs['exponent'] = '-1' if partno > 1
        units.add_element('unit', attrs)
      }
      return
    end

    attr =
      {
        'substance' => { 'units' => 'mole'},
        'volume'    => { 'units' => 'litre'},
        'area'      => { 'units' => 'metre', 'exponent' => '3'},
        'length'    => { 'units' => 'metre'},
        'time'      => { 'units' => 'second'}
      }[name]

    raise SBMLError.new('Unknown unit requested: %s' % name) if attr.nil?
    
    units.add_element('unit', attr)
  end

  def process_functions
    @functions = []

    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfFunctionDefinitions/functionDefinition') { |el|
      name = el.attribute('id').value
      lambda = REXML::XPath.first(el, 'math/lambda')
      raise SBMLError.new("Expected MathML lambda inside functionDefinition") if lambda.nil?
      f = { :bvars => [], :expr => REXML::XPath.first(lambda, 'apply or ci')}
      raise SBMLError.new("Expected an apply or ci element in MathML lambda") if f[:expr].nil?
      REXML::XPath.each(lambda, 'bvar/ci') { |bvarci|
        f[:bvars].push(bvarci.text.strip)
      }
    }
  end

  def expand_all_functions(el)
    clone_el = el.deep_clone
    REXML::XPath.each(clone_el, 'apply[ci[position()=1]]') { |apply|
      new_el = expand_function(el)
      context = el.parent
      context.remove_element(el)
      context.add_element(new_el)
    }
    clone_el
  end

  def expand_function(apply)
    args = REXML::XPath.match(apply, 'ci or cn or csymbol or apply or exponentiale or imaginaryi or notanumber or true or false or emptyset or pi or eulergamma or infinity')
    return apply unless args[0].name == 'ci'
    f = @functions[args[0].text.strip]
    raise SBMLError.new("Attempt to apply a function not defined in listOfFunctionDefinitions") if f.nil?
    raise SBMLError.new("Incorrect number of arguments for user-defined function application") unless f[:bvars].size + 1 == args.size
    expr = apply.deep_clone
    REXML::XPath.each(expr, 'ci') { |el|
      index = f[:bvars].index(el.text.strip)
      if not index.nil?
        container = el.parent
        container.delete_element(el)
        container.add_element(args[index + 1].deep_clone)
      end
    }
    expr
  end

  def process_parameters
    @parameters = []
    paramComponent = @cellmlEl.add_element('component', {'name' => 'parameters'})

    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfParameters/parameter') { |el|
      attrs = {'name' => el.attribute('id').value}
      attrs['initial_value'] = el.attribute('value') unless el.attribute('value').nil?
      attrs['units'] = el.attribute('units') unless el.attribute('units').nil?

      @parameters.push(el.attribute('id'))
      paramComponent.add_element('variable', attrs)
    }

    @cellmlEl.delete_element(paramComponent) if @parameters.empty?
  end

  def compartment_spatial_units(el)
    spatialDim = el.attribute('spatialDimensions')
    if spatialDim.nil?
      spatialDim = 3
    else
      spatialDim = spatialDim.value.to_i
    end
    
    unitsAt = el.attribute('units')
    if unitsAt.nil?
      units = case spatialDim
              when 3 then 'volume'
              when 2 then 'area'
              when 1 then 'length'
              when 0 then nil
              else raise SBMLError.new('Unsupported spatial dimension %u' % spatialDim)
              end
    else
      units = unitsAt.value
    end

    ensure_builtin_unit(units) if not units.nil?

    units
  end

  def species_substance_units(el)
    unitsAt = el.attribute('substanceUnits')
    units = case when unitsAt.nil? then 'substance' else unitsAt.value end
    ensure_builtin_unit(units)
    units
  end

  def species_concentration_units(species, compartment)
    units = species_substance_units(species) + '_per_' + compartment_spatial_units(compartment)
    ensure_builtin_unit(units)
    units
  end

  def process_compartments
    @compartments = {}

    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfCompartments/compartment') { |el|
      component = @cellmlEl.add_element('component', {'name' => el.attribute('id').value })
      @compartments[el.attribute('id').value] = { :component => component, :compartment => el }

      units = compartment_spatial_units(el)

      if not el.nil?
        varAttrs = {'name' => 'size', 'units' => units}
        size = el.attribute('size')
        if not size.nil?
          varAttrs['initial_value'] = size.value
        end

        component.add_element('variable', varAttrs)
      end
    }
  end

  def add_math_apply_eq(component, varname)
    math = component.add_element('math')
    math.add_namespace(MATHML_NS)
    apply = math.add_element('apply')
    apply.add_element('eq')
    apply.add_element('ci').add_text(varname)
    yield(apply) if block_given?
    apply
  end

  def compute_initial_value(component, varname, units)
    ivvar = varname + '_initial'
    component.add_element('variable', {'name' => ivvar, 'units' => units})

    add_math_apply_eq(component, ivvar) { |apply| yield(apply) }

    ivvvar
  end

  def process_species
    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfSpecies/species') { |el|
      compartment = @compartments[el.attribute('compartment').value]
      name = el.attribute('id').value

      onlySubstanceUnits = false
      hasOnlySubstanceUnitsAt = el.attribute('hasOnlySubstanceUnits')
      onlySubstanceUnits = (hasOnlySubstanceUnitsAt.value == 'true') unless hasOnlySubstanceUnitsAt.nil?

      units = onlySubstanceUnits ? species_substance_units(el) : species_concentration_units(el, compartment[:compartment])
      attrs = {'name' => name, 'units' => units }

      initialConcentrationAt = el.attribute('initialConcentration')
      initialAmountAt = el.attribute('initialAmount')

      if not initialConcentrationAt.nil?
        if onlySubstanceUnits
          attrs['initial_value'] =
            compute_initial_value(compartment[:component], name, units) { |apply|
              atimes = apply.add_element('apply')
              atimes.add_element('times')
              cn = atimes.add_element('cn', {'cellml:units' => species_concentration_units(el, compartment[:compartment])}).
                     add_text(initialConcentrationAt.value)
              atimes.add_element('ci').add_text('size')
            }
        else
          attrs['initial_value'] = initialConcentrationAt.value
        end
      else
        if not initialAmountAt.nil?
          if not onlySubstanceUnits
            attrs['initial_value'] =
              compute_initial_value(compartment[:component], name, units) { |apply|
                adiv = apply.add_element('apply')
                adiv.add_element('divide')
                cn = adiv.add_element('cn', {'cellml:units' => species_substance_units(el)}).
                       add_text(initialConcentrationAt.value)
                adiv.add_element('ci').add_text('size')
              }
          else
            attrs['initial_value'] = initialAmountAt.value
          end
        else
          # We don't have an initial amount or initial concentration. We might
          # have an initial assignment or a rule...
        end
      end

      if not initialConcentrationAt.nil?
        attrs['initial_value'] = initialConcentrationAt.value
      else
        attrs['initial_value'] = el.attribute('id').value + '_initial'
        if not initialAmountAt.nil?
        end
      end

      species = compartment[:component].add_element('variable', attrs)
    }
  end
end

begin
  SBMLToCellML.convert(REXML::Document.new(File.new(ARGV[0]))).write(STDOUT, 1)
  puts ""
rescue SBMLError
  STDERR.puts "Error: %s" % $!.why
end

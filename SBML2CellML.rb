#!/usr/bin/env ruby

require 'rexml/document'

MathExpressionChild = '*[self::ci or self::cn or self::csymbol or self::apply or self::exponentiale or self::imaginaryi or self::notanumber or self::true or self::false or self::emptyset or self::pi or self::eulergamma or self::infinity or self::piecewise]'
MathAnyChild = '*[self::cn or self::ci or self::csymbol or self::apply or self::reln or self::fn or self::interval or self::inverse or self::sep or self::condition or self::declare or self::lambda or self::compose or self::ident or self::domain or self::codomain or self::image or self::domainofapplication or self::piecewise or self::piece or self::otherwise or self::quotient or self::factorial or self::divide or self::max or self::min or self::minus or self::power or self::rem or self::times or self::root or self::gcd or self::and or self::or or self::xor or self::not or self::implies or self::forall or self::exists or self::abs or self::conjugate or self::arg or self::real or self::imaginary or self::lcm or self::floor or self::ceiling or self::eq or self::neq or self::gt or self::lt or self::geq or self::leq or self::equivalent or self::approx or self::factorof or self::int or self::diff or self::partialdiff or self::lowlimit or self::uplimit or self::bvar or self::degree or self::divergence or self::grad or self::curl or self::laplacian or self::set or self::list or self::union or self::intersect or self::in or self::notin or self::subset or self::prsubset or self::notsubset or self::notprsubset or self::setdiff or self::card or self::cartesianproduct or self::sum or self::product or self::limit or self::tendsto or self::exp or self::ln or self::log or self::mean or self::sdev or self::variance or self::median or self::mode or self::moment or self::momentabout or self::vector or self::matrix or self::matrixrow or self::determinant or self::transpose or self::selector or self::vectorproduct or self::scalarproduct or self::outerproduct or self::integers or self::reals or self::rationals or self::naturalnumbers or self::complexes or self::primes or self::exponentiale or self::imaginaryi or self::notanumber or self::true or self::false or self::emptyset or self::pi or self::eulergamma or self::infinity or self::plus]'

class SBMLError < Exception
  def initialize(why)
    @why = why
  end

  def why
    @why
  end
end

class BaseUnits
  def initialize(name)
    @name = name
  end

  attr_reader :name
end

class BaseUnitInstance
  def initialize(baseUnit, exponent)
    @baseUnit = baseUnit
    @exponent = exponent
  end

  def ==(x)
    return false unless x.kind_of?(BaseUnitInstance)
    return false unless x.exponent == @exponent
    return false unless x.baseUnit.name == @baseUnit.name
    true
  end

  attr_reader :baseUnit, :exponent
end

class Units
  BAmpere = BaseUnits.new('ampere')
  BCandela = BaseUnits.new('candela')
  BKelvin = BaseUnits.new('kelvin')
  BKilogram = BaseUnits.new('kilogram')
  BMetre = BaseUnits.new('metre')
  BMole = BaseUnits.new('mole')
  BSecond = BaseUnits.new('second')

  def initialize(unitsList, multiplier)
    @unitsList = unitsList.sort { |x,y| x.baseUnit.name <=> y.baseUnit.name }
    @multiplier = multiplier
  end
  attr_reader :unitsList
  attr_reader :multiplier

  def merge_with(other_units, self_exponent = 1.0, other_exponent = 1.0, self_multiplier = 1.0, other_multiplier = 1.0)
    if other_units.nil?
      other_units = []
    elsif other_units.kind_of?(Units)
      other_multiplier *= other_units.multiplier
      other_units = other_units.unitsList
    elsif other_units.kind_of?(BaseUnits)
      other_units = [BaseUnitsInstance.new(other_units, 1)]
    end

    combined = Hash.new(0)
    @unitsList.each { |x|
      combined[x.baseUnit] = combined[x.baseUnit] + x.exponent*self_exponent
    }
    other_units.each { |x|
      combined[x.baseUnit] = combined[x.baseUnit] + x.exponent*other_exponent
    }
    Units.new(combined.reject { |k,v| v==0 }.map { |k,v| BaseUnitInstance.new(k, v) },
              other_multiplier**other_exponent * (self_multiplier * @multiplier)**self_exponent)
  end

  # The factor to multiply units of this type by to convert to other_units...
  def conversion_factor(other_units)
    @multiplier.to_f / other_units.multiplier.to_f
  end

  Ampere = Units.new([BaseUnitInstance.new(BAmpere, 1.0)], 1.0)
  Becquerel = Units.new([BaseUnitInstance.new(BSecond, -1)], 1.0)
  Candela = Units.new([BaseUnitInstance.new(BCandela, 1)], 1.0)
  # Not in SBML...
  # Celsius = Units.new(BaseUnitInstance.new(BKelvin, 1, -273.15))
  Coulomb = Units.new([BaseUnitInstance.new(BAmpere, 1), BaseUnitInstance.new(BSecond, 1)], 1.0)
  Dimensionless = Units.new([], 1.0)
  Farad  = Units.new([BaseUnitInstance.new(BMetre, -2), BaseUnitInstance.new(BKilogram, -1),
                      BaseUnitInstance.new(BSecond, 4), BaseUnitInstance.new(BAmpere, 2)], 1.0)
  Gram = Units.new([BaseUnitInstance.new(BKilogram, 1)], 1000.0)
  Gray = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BSecond, -2)], 1.0)
  Henry = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BKilogram, 1),
                     BaseUnitInstance.new(BSecond, -2), BaseUnitInstance.new(BAmpere, -2)], 1.0)
  Hertz = Units.new([BaseUnitInstance.new(BSecond, -1)], 1.0)
  Joule = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BKilogram, 1),
                     BaseUnitInstance.new(BSecond, -2)], 1.0)
  Katal = Units.new([BaseUnitInstance.new(BSecond, -1), BaseUnitInstance.new(BMole, 1)], 1.0)
  Kelvin = Units.new([BaseUnitInstance.new(BKelvin, 1)], 1.0)
  Kilogram = Units.new([BaseUnitInstance.new(BKilogram, 1)], 1.0)
  Liter = Units.new([BaseUnitInstance.new(BMetre, 3)], 1000.0)
  Litre = Units.new([BaseUnitInstance.new(BMetre, 3)], 1000.0)
  Lumen = Units.new([BaseUnitInstance.new(BCandela, 1)], 1.0)
  Lux = Units.new([BaseUnitInstance.new(BCandela, 1), BaseUnitInstance.new(BMetre, -2)], 1.0)
  Meter = Units.new([BaseUnitInstance.new(BMetre, 1)], 1.0)
  Metre = Units.new([BaseUnitInstance.new(BMetre, 1)], 1.0)
  Mole = Units.new([BaseUnitInstance.new(BMole, 1)], 1.0)
  Newton = Units.new([BaseUnitInstance.new(BMetre, 1), BaseUnitInstance.new(BKilogram, 1),
                      BaseUnitInstance.new(BSecond, -2)], 1.0)
  Ohm = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BKilogram, 1),
                   BaseUnitInstance.new(BSecond, -3), BaseUnitInstance.new(BAmpere, -2)], 1.0)
  Pascal = Units.new([BaseUnitInstance.new(BMetre, -1), BaseUnitInstance.new(BKilogram, 1),
                      BaseUnitInstance.new(BSecond, -2)], 1.0)
  Radian = Units.new([], 1.0)
  Second = Units.new([BaseUnitInstance.new(BSecond, 1)], 1.0)
  Siemens = Units.new([BaseUnitInstance.new(BMetre, -2), BaseUnitInstance.new(BKilogram, -1),
                       BaseUnitInstance.new(BSecond, 3), BaseUnitInstance.new(BAmpere, 2)], 1.0)
  Sievert = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BSecond, -2)], 1.0)
  Steradian = Units.new([], 1.0)
  Tesla = Units.new([BaseUnitInstance.new(BKilogram, 1),
                     BaseUnitInstance.new(BSecond, -2),
                     BaseUnitInstance.new(BAmpere, -1)], 1.0)
  Volt = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BKilogram, 1),
                    BaseUnitInstance.new(BSecond, -3), BaseUnitInstance.new(BAmpere, -1)], 1.0)
  Watt = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BKilogram, 1),
                    BaseUnitInstance.new(BSecond, -3)], 1.0)
  Weber = Units.new([BaseUnitInstance.new(BMetre, 2), BaseUnitInstance.new(BKilogram, 1),
                     BaseUnitInstance.new(BSecond, -2), BaseUnitInstance.new(BAmpere, -1)], 1.0)
  # SBML only...
  Item = Units.new([BaseUnitInstance.new(BMole, 1)], 1.66053878233550918318605E-24)

  SBMLUnits = {
    "coulomb" => Coulomb, "dimensionless" => Dimensionless,
    "farad " => Farad, "gram" => Gram, "gray" => Gray,
    "henry" => Henry, "hertz" => Hertz,
    "joule" => Joule, "katal" => Katal,
    "kelvin" => Kelvin, "kilogram" => Kilogram,
    "liter" => Liter, "litre" => Litre,
    "lumen" => Lumen, "lux" => Lux,
    "meter" => Meter, "metre" => Metre,
    "mole" => Mole, "newton" => Newton,
    "ohm" => Ohm, "pascal" => Pascal,
    "radian" => Radian, "second" => Second,
    "siemens" => Siemens, "sievert" => Sievert,
    "steradian" => Steradian, "tesla" => Tesla,
    "volt" => Volt, "watt" => Watt,
    "weber" => Weber, "item" => Item
  }

  def self.sbmlUnit(name)
    SBMLUnits[name]
  end

  def self.sbmlUnitByType(utype)
    # There is more than one equivalent to dimensionless, force dimensionless...
    return "dimensionless" if utype === Dimensionless
    kv = SBMLUnits.find { |k,v| v === utype }
    return nil if kv.nil?
    kv[0]
  end

  def ===(units)
    return false unless units.kind_of?(Units)
    return false unless units.multiplier == @multiplier or @unitsList.length==0
    return false unless @unitsList == units.unitsList
    true
  end
end

class DefinedUnitsSet
  def initialize(cellmlEl)
    @definedUnits = {}
    @cellmlEl = cellmlEl
    @unitNo = 0
  end

  def unitsDefined(name, utype)
    @definedUnits[name] = utype
  end

  def findUnits(name)
    builtin = Units.sbmlUnit(name)
    return builtin unless builtin.nil? or name=="item"

    @definedUnits[name]
  end

  def findOrMakeUnits(utype)
    builtin = Units.sbmlUnitByType(utype)
    # Item is not in CellML so must be explicitly defined.
    return builtin unless builtin.nil? or utype=="item"

    @definedUnits.each_pair { |k,v|
      return k if v===utype
    }

    name = 's2cunit_%u' % @unitNo

    @unitNo = @unitNo + 1
    el = @cellmlEl.add_element('units', {'name' => name})

    first = true
    utype.unitsList.each { |u|
      attrs = {}
      if first
        first = false
        attrs['multiplier'] = utype.multiplier if utype.multiplier != 1.0
      end

      attrs['units'] = u.baseUnit.name
      attrs['exponent'] = u.exponent if u.exponent != 1.0
      el.add_element('unit', attrs)
    }

    @definedUnits[name] = utype

    name
  end
end

class UnitInferenceEngine
  def initialize(maths, unitSet, comps)
    @maths = maths
    @unitSets = unitSet
    @comps = comps
    @inferred = {}
    @rules = []

    # Set up the rules for inferring units...

    apply_rule (['max', 'min', 'minus', 'plus','abs', 'floor', 'ceiling']) { |apply, children|
      # Arguments and roots are of the same type.
      next if children.inject(!@inferred[apply].nil?) { |m,c| m and not @inferred[c].nil? }

      units = nil
      if not @inferred[apply].nil?
        units = @inferred[apply]
      else
        children.each { |x|
          if not @inferred[x].nil?
            units = @inferred[x]
            break
          end
        }
      end
      next if units.nil?
      @did_work = true
      @inferred[apply] = units
      children.each { |x| @inferred[x] = units }
    }

    apply_rule (['eq', 'neq', 'gt', 'lt', 'geq', 'leq', 'equivalent', 'approx']) { |apply, children|
      # Arguments are of the same type...
      next if children.inject(true) { |m,c| m and not @inferred[c].nil? }
      utype = nil
      children.each { |x|
        if not @inferred[x].nil?
          utype = @inferred[x]
          break
        end
      }
      next if utype.nil?
      @did_work = true
      children.each { |x| @inferred[x] = utype }
    }

    apply_rule (['factorial', 'gcd', 'lcm', 'factorof', 'exp', 'ln', 'log']) { |apply, children|
      # Both the child and arguments are dimensionless...
      next if children.inject(!@inferred[apply].nil?) { |m,c| m and not @inferred[c].nil? }
      @did_work = true
      @inferred[apply] = Units::Dimensionless
      children.each { |c| @inferred[c] = Units::Dimensionless }
    }

    apply_rule (['times']) { |apply, children|
      # Exactly one of the parent and children have unknown dimensions...
      next unless (children + [apply]).inject(0) { |m,c|
        case @inferred[c].nil? when true then m + 1 else m end
      } == 1
      @did_work = true

      units = Units::Dimensionless
      leftout = nil
      children.each { |c|
        if @inferred[c].nil?
          leftout = c
        else
          units = units.merge_with(@inferred[c])
        end
      }

      if leftout.nil?
        @inferred[apply] = units
      else
        # Divide parent units by the child units to get the leftout child units.
        @inferred[leftout] = @inferred[apply].merge_with(units, 1.0, -1.0)
      end
    }

    apply_rule(['quotient', 'divide', 'rem']) { |apply, children|
      # Exactly one of the parent and children have unknown dimensions...
      next unless (children + [apply]).inject(0) { |m,c|
        case @inferred[c].nil? when true then m + 1 else m end
      } == 1

      @did_work = true

      units = Units::Dimensionless
      exponent = 1.0
      leftout = nil
      leftoutExponent = 1.0
      children.each { |c|
        if @inferred[c].nil?
          leftout = c
          leftoutExponent = exponent
        else
          units = units.merge_with(@inferred[c], 1.0, exponent)
        end
        exponent = -1.0
      }
      if leftout.nil?
        @inferred[apply] = units
      else
        # Divide parent units by the child units to get the leftout child units.
        @inferred[leftout] = @inferred[apply].merge_with(units, exponent, -exponent)
      end
    }

    apply_rule (['power']) { |apply,children|
      if (@inferred[children[1]].nil?)
        @did_work = true
        @inferred[children[1]] = Units::Dimensionless
      end

      next unless @inferred[children[0]].nil? ^ @inferred[apply].nil?

      # Is children[1] a constant?
      # Note that we could be smarter and compute constants here.
      next unless children[1].name == 'cn'
      exponent = children[1].text.strip.to_f
      
      @did_work = true

      if @inferred[children[0]].nil?
        @inferred[children[0]] = @inferred[apply].merge_with(nil, -exponent)
      else
        @inferred[apply] = @inferred[children[0]].merge_with(nil, exponent)
      end
    }

    apply_rule (['root']) { |apply,children|
      degs = REXML::XPath.match(apply, "degree/" + MathExpressionChild)
      if degs.length == 0
        degree = 2.0
      else
        if @inferred[degs[0]].nil?
          @did_work = true
          @inferred[degs[0]] = Units::Dimensionless
        end
        
        # We can only proceed if we have a constant...
        next unless degs[0].name == 'cn'
        degree = degs[0].text.strip.to_f
      end

      next unless @inferred[children[0]].nil? ^ @inferred[apply].nil?

      @did_work = true
      exponent = 1.0 / degree

      if @inferred[children[0]].nil?
        @inferred[children[0]] = @inferred[apply].merge_with(nil, -exponent)
      else
        @inferred[apply] = @inferred[children[0]].merge_with(nil, exponent)
      end
    }

    apply_rule (['int']) { |apply,children|
      # bvar, lowlimit, uplimit all have the same units...
      boundtypes = REXML::XPath.match(apply, '(bvar or lowlimit or uplimit)/' + MathExpressionChild)
      boundtype = boundtypes.find { |el| next if @inferred[el].nil?; break @inferred[el]}
      if not boundtype.nil?
        boundtypes.each { |el|
          if @inferred[el].nil?
            @did_work = true
            @inferred[el] = boundtype
          end
        }
      end

      # Exactly one of the three unit positions must be unknown...
      next unless [@inferred[apply], @inferred[children[0]], boundtype].inject(0) { |m,v|
        m + case v.nil? when true then 1 else 0 end
      } == 1

      @did_work = true

      if @inferred[apply].nil?
        @inferred[apply] = @inferred[children[0]].merge_with(boundtype)
      elsif @inferred[children[0]].nil?
        @inferred[children[0]] = @inferred[apply].merge_with(boundtype, 1.0, -1.0)
      else
        boundtype = @inferred[apply].merge_with(@inferred[children[0]], 1.0, -1.0)
        boundtypes.each { |el|
          @inferred[el] = boundtype
        }
      end
    }

    apply_rule (['diff','partialdiff']) { |apply,children|
      # XXX partialdiff when >1 type involved...
      boundtypes = REXML::XPath.match(apply, 'bvar/' + MathExpressionChild)
      boundtype = @inferred[boundtypes[0]]

      degree = 1.0
      REXML::XPath.each(apply, '(bvar/degree/' + MathExpressionChild +
                        ') or degree/' + MathExpressionChild) { |deg|
        if @inferred[deg].nil?
          @did_work = true
          @inferred[deg] = Units::Dimensionless
        end
        next unless deg.name == 'cn'
        degree = deg.text.strip.to_f
      }

      # Exactly one of the three unit positions must be unknown...
      next unless [@inferred[apply], @inferred[children[0]], boundtype].inject(0) { |m,v|
        m + case v.nil? when true then 1 else 0 end
      } == 1

      @did_work = true
      
      if @inferred[apply].nil?
        @inferred[apply] = @inferred[children[0]].merge_with(boundtype, 1.0, -degree)
      elsif @inferred[children[0]].nil?
        @inferred[children[0]] = @inferred[apply].merge_with(boundtype, 1.0, degree)
      else
        @inferred[boundtypes[0]] = @inferred[children[0]].merge_with(@inferred[apply], 1.0/degree, -1.0/degree)
      end
    }

    complex_rule ("descendant-or-self::piecewise") { |piecewise,children|
      otherwise = REXML::XPath.first(piecewise, 'otherwise/' + MathExpressionChild)
      match = [piecewise, otherwise] + REXML::XPath.match(piecewise, 'piece').map { |piece|
        REXML::XPath.first(piece, MathExpressionChild)
      }

      # Everything in match has the same type. At least one must be unknown...
      next if match.find { |x| @inferred[x].nil? }.nil?
      # and at least one must be known...
      known = match.find { |x| not @inferred[x].nil? }
      next if known.nil?
      utype = @inferred[known]
      @did_work = true
      match.each { |x| @inferred[x] = utype }
    }
  end

  def known_root(utype)
    if (@maths.name == "math")
      root = REXML::XPath.first(@maths, MathExpressionChild)
    else
      root = @maths
    end
    @inferred[root] = utype
  end

  def infer
    # Start from known variables...
    known_variables
    dimensionless_constants

    @did_work = true
    while (@did_work)
      @did_work = false
      
      @rules.each { |xpath,proc|
        REXML::XPath.each(@maths, xpath) { |apply|
          proc.call(apply, REXML::XPath.match(apply, MathExpressionChild))
        }
      }
    end
  end

  def set_constant_units
    REXML::XPath.each(@maths, 'descendant-or-self::cn') { |cn|
      cn.add_attribute('cellml:units', @unitSets.findOrMakeUnits(@inferred[cn])) unless @inferred[cn].nil?
    }
  end

  private
  def apply_rule(applytypes, &block)
    @rules.push(['descendant-or-self::apply[' + applytypes.map { |x| x}.join(' or ') + ']',
                block])
  end

  def complex_rule(xslt, &block)
    @rules.push([xslt, block])
  end

  def known_variables
    REXML::XPath.each(@maths, "descendant::ci") { |ci|
      @inferred[ci] = @unitSets.findUnits(@comps.units(ci.text.strip))
    }
  end

  def dimensionless_constants
    REXML::XPath.each(@maths, 'descendant::exponentiale or descendant::imaginaryi or descendant::notanumber ' +
                      'or descendant::pi or descendant::eulergamma or descendant::infinity') { |el|
      @inferred[el] = Units::Dimensionless
    }
  end
end

class ComponentConnector
  def initialize(cellmlEl)
    @cellmlEl = cellmlEl
    @varreg = {}
    @contexts = []
  end

  def push_context
    @contexts.push(@varreg.clone)
  end

  def pop_context
    @varreg = @contexts.pop
  end

  def register_variable(name, component, variable, units)
    raise "Invalid type of units" unless units.kind_of?(String)
    @varreg[name] = [component, variable, units]
  end

  def units(name)
    @varreg[name][2]
  end

  def connect(name, component, component2 = nil, units = nil)
    (component2, name2, units) = @varreg[name] if component2.nil?
    
    return units if component == component2

    # Look for an existing connection...
    conn = REXML::XPath.match(@cellmlEl, "/model/connection[map_components[@component_1=\"#{component}\" and " +
                              "@component_2=\"#{component2}\"]]")
    return connect_vars(conn[0], name, name2, component, name, units) unless conn.size==0

    conn = REXML::XPath.match(@cellmlEl, "/model/connection[map_components[@component_1=\"#{component2}\" and " +
                              "@component_2=\"#{component}\"]]")
    return connect_vars(conn[0], name2, name, component, name, units) unless conn.size==0

    conn = @cellmlEl.add_element('connection')
    conn.add_element('map_components', {'component_1' => component, 'component_2' => component2})
    connect_vars(conn, name, name2, component, name, units)
  end

  def connect_vars(conn, name1, name2, varcomp, varname, units)
    return unless REXML::XPath.match(conn, "map_variables[@variable_1=\"#{name1}\" " +
                                     "and @variable_2=\"#{name2}\"]").size==0

    conn.add_element('map_variables', {'variable_1' => name1, 'variable_2' => name2})

    REXML::XPath.each(@cellmlEl, "/model/component[@name=\"#{varcomp}\"]") { |el|
      el.add_element('variable', {'name' => varname, 'public_interface' => 'in',
                                  'units' => units})
    }

    return units
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
    @units = DefinedUnitsSet.new(@cellmlEl)
    @comps = ComponentConnector.new(@cellmlEl)
    translate_sbml
  end

  attr_reader :sbml, :cellml

  private
  def create_cellml
    @cellml = REXML::Document.new()
    @cellmlEl = @cellml.add_element('model')
    @cellmlEl.add_namespace('cellml', CELLML_NS)
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

    id = REXML::XPath.first(@sbmlEl, '/sbml/model').attribute('id')
    @cellmlEl.add_attribute('name', id.value) unless id.nil?

    process_units
    setup_time
    process_functions
    process_parameters
    process_compartments
    process_species
    process_rules
    process_reactions
  end

  def setup_time
    ensure_builtin_unit('time')
    @cellmlEl.add_element('component', {'name' => 'environment'}).
      add_element('variable', {'name'=>'time', 'public_interface' => 'out',
                               'units' => 'time'})
    @comps.register_variable('time', 'environment', 'time', 'time')
  end

  def define_item
    return if @item_defined
    @item_defined = true
    @cellmlEl.add_element('units', {'name' => 'item'}).
      add_element('unit', {'units' => 'mole', 'exponent' => -24,
                  'multiplier' => '1.66053878233550918318605'})
    @units.unitsDefined("item", Units::Item)
  end

  def process_units
    @item_defined = false
    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfUnitDefinitions/unitDefinition') { |el|
      utype = nil
      name = el.attribute('id').value
      units = @cellmlEl.add_element('units', {'name' => name})
      REXML::XPath.each(el, 'listOfUnits/unit') { |u|
        kind = u.attribute('kind').value
        define_item if kind == "item"
        attrs = {'units' => kind }
        {'exponent' => 'exponent', 'scale' => 'prefix', 'multiplier' => 'multiplier'}.each_pair { |sbml,cellml|
          attrs[cellml] = u.attribute(sbml).value unless u.attribute(sbml).nil?
        }
        units.add_element('unit', attrs)

        def floatattr(x, default)
          return default if x.nil?
          x.value.to_f
        end

        exponent = 
        if utype.nil?
          utype = @units.findUnits(kind).merge_with(nil,
                                                    floatattr(u.attribute('exponent'), 1.0),
                                                    0.0,
                                                    floatattr(u.attribute('multiplier'), 1.0) *
                                                    (10.0**floatattr(u.attribute('scale'), 0.0)))
        else
          utype = utype.merge_with(@units.findUnits(kind), 1.0,
                                   floatattr(u.attribute('exponent'), 1.0),
                                   1.0,
                                   floatattr(u.attribute('multiplier'), 1.0) *
                                   (10.0**floatattr(u.attribute('scale'), 0.0)))
        end
      }
      if not utype.nil?
        @units.unitsDefined(name, utype)
      end
    }
  end

  def ensure_builtin_unit(name)
    return unless REXML::XPath.first(@cellmlEl, "/model/units[@name=\"#{name}\"]").nil?

    units = @cellmlEl.add_element('units', {'name' => name})

    spl = name.split('_per_')
    if (spl.length > 1)
      utype = nil
      spl.each { |part|
        ensure_builtin_unit(part)
        attrs = {'units' => part}
        if utype.nil?
          utype = @units.findUnits(part)
        else
          utype = utype.merge_with(@units.findUnits(part), 1.0, -1.0)
          attrs['exponent'] = '-1'
        end
        units.add_element('unit', attrs)
      }
      @units.unitsDefined(name, utype)
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

    if name == 'area'
      @units.unitsDefined(name, Units.new([BaseUnitInstance.new(Units::Metre, 3.0)], 1.0))
    else
      type = {
        'substance' => Units::Mole,
        'volume'    => Units::Litre,
        'length'    => Units::Metre,
        'time'      => Units::Second
      }[name]
      @units.unitsDefined(name, type)
    end
  end

  def process_functions
    @functions = {}

    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfFunctionDefinitions/functionDefinition') { |el|
      name = el.attribute('id').value
      lambda = REXML::XPath.first(el, 'math/lambda')
      raise SBMLError.new("Expected MathML lambda inside functionDefinition") if lambda.nil?
      f = { :bvars => [], :expr => REXML::XPath.first(lambda, 'apply or ci')}
      raise SBMLError.new("Expected an apply or ci element in MathML lambda") if f[:expr].nil?
      REXML::XPath.each(lambda, 'bvar/ci') { |bvarci|
        f[:bvars].push(bvarci.text.strip)
      }
      @functions[name] = f
    }
  end

  def fix_maths(el, component, target_units)
    el = expand_all_functions(el)

    # Make a CellML style time where we encounter the SBML csymbol
    REXML::XPath.match(el, "descendant::csymbol[@definitionURL='http://www.sbml.org/sbml/symbols/time']").each { |csym|
      ci = REXML::Element.new('ci')
      ci.add_text('time')
      csym.parent.replace_child(csym, ci)
    }

    # Connect up all the ci elements to this component...
    REXML::XPath.each(el, "descendant::ci") { |ci|
      @comps.connect(ci.text.strip, component)
    }

    # Run the units inference engine...
    uie = UnitInferenceEngine.new(el, @units, @comps)
    uie.known_root(target_units) unless target_units.nil?
    uie.infer
    uie.set_constant_units

    el
  end

  def expand_all_functions(el)
    clone_el = el.deep_clone

    good = true
    while good
      catch (:restart) do
        REXML::XPath.match(clone_el, 'descendant::apply[ci]').each { |apply|
          new_el = expand_function(apply)
          next if new_el == apply
          apply.parent.replace_child(apply, new_el)
          throw :restart
        }
        good = false
      end
    end
    clone_el
  end

  def expand_function(apply)
    args = REXML::XPath.match(apply, MathAnyChild)
    return apply unless (not args[0].nil?) and args[0].name == 'ci'
    f = @functions[args[0].text.strip]
    raise SBMLError.new("Attempt to apply a function not defined in listOfFunctionDefinitions") if f.nil?
    raise SBMLError.new("Incorrect number of arguments for user-defined function application") unless f[:bvars].size + 1 == args.size
    expr = f[:expr].deep_clone
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
      name = el.attribute('id').value
      attrs = {'name' => name, 'public_interface' => 'out'}
      attrs['initial_value'] = el.attribute('value').value unless el.attribute('value').nil?
      if el.attribute('units').nil?
        units = 'unknown'
      else
        units = el.attribute('units').value
      end

      attrs['units'] = units

      @parameters.push(el.attribute('id'))
      var = paramComponent.add_element('variable', attrs)
      @comps.register_variable(name, 'parameters', name, units)

      process_initial_assignment(name, units, paramComponent, var)
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
      name = el.attribute('id').value
      component = @cellmlEl.add_element('component', {'name' => name })
      units = compartment_spatial_units(el)
      @compartments[el.attribute('id').value] = { :name => name, :component => component, :compartment => el,
                                                  :units => @units.findUnits(units) }

      varAttrs = {'name' => 'size', 'units' => units, 'public_interface' => 'out'}
      size = el.attribute('size')
      
      @comps.register_variable(name, name, 'size', units)

      if not size.nil?
        varAttrs['initial_value'] = size.value
      end
      
      var = component.add_element('variable', varAttrs)

      if size.nil?
        process_initial_assignment(name, units, component, var)
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

    ivvar
  end

  def process_species
    @species = {}
    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfSpecies/species') { |el|
      compartment = @compartments[el.attribute('compartment').value]
      name = el.attribute('id').value

      onlySubstanceUnits = false
      hasOnlySubstanceUnitsAt = el.attribute('hasOnlySubstanceUnits')
      onlySubstanceUnits = (hasOnlySubstanceUnitsAt.value == 'true') unless hasOnlySubstanceUnitsAt.nil?

      units = onlySubstanceUnits ? species_substance_units(el) : species_concentration_units(el, compartment[:compartment])
      attrs = {'name' => name, 'units' => units, 'public_interface' => 'out' }

      initialConcentrationAt = el.attribute('initialConcentration')
      initialAmountAt = el.attribute('initialAmount')

      consider_external_assignment = false

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
                       add_text(initialAmountAt.value)
                adiv.add_element('ci').add_text('size')
              }
          else
            attrs['initial_value'] = initialAmountAt.value
          end
        else
          # We don't have an initial amount or initial concentration. We might
          # have an initial assignment or a rule...
          consider_external_assignment = true
        end
      end

      species = compartment[:component].add_element('variable', attrs)
      @comps.register_variable(name, compartment[:name], name, units)

      @species[name] = { :onlySubstance => onlySubstanceUnits,
                         :units => @units.findUnits(units), :compartment => compartment }

      if consider_external_assignment
        process_initial_assignment(name, units, compartment[:component], species)
        # It might also be a rule...
      end
    }
  end

  def process_initial_assignment(name, units, component, var)
    smath = REXML::XPath.first(@sbmlEl, "/sbml/model/listOfInitialAssignments/" +
                              "initialAssignment[@symbol=\"#{name}\"]/math")
    return false if smath.nil?

    units = @units.findUnits(units)
    smath = fix_maths(smath, component, units)

    ivname = name + '_initial'
    var.add_attribute('initial_value', ivname)
    component.add_element('variable', {'name' => ivname, 'units' => units})
    add_math_apply_eq(component, ivname) { |applyeq|
      apply = REXML::XPath.first(smath, MathExpressionChild)
      applyeq.add_element(apply)
    }
    true
  end

  def process_rules
    ruleComponent = @cellmlEl.add_element('component', {'name' => 'rules'})

    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfRules/*[self::assignmentRule or self::algebraicRule or self::rateRule]') { |el|
      if el.name == 'algebraicRule'
        smath = REXML::XPath.first(el, 'math')
        smath = fix_maths(smath, 'rules', nil)
        ruleComponent.add_element(smath)
      else
        math = ruleComponent.add_element('math')
        math.add_namespace(MATHML_NS)
        applyeq = math.add_element('apply')
        applyeq.add_element('eq')
        varname = el.attribute('variable').value
        if el.name == 'assignmentRule'
          applyeq.add_element('ci').add_text(varname)
          units = @units.findUnits(@comps.connect(varname, 'rules'))
        else
          adiff = applyeq.add_element('apply')
          adiff.add_element('diff')
          adiff.add_element('bvar').add_element('ci').add_text('time')
          adiff.add_element('ci').add_text(varname)
          units = @units.findUnits(@comps.connect(varname, 'rules')).
            merge_with(@units.findUnits('time'), 1.0, -1.0)
        end

        sexpr = REXML::XPath.first(el, 'math/' + MathExpressionChild)
        applyeq.add_element(fix_maths(sexpr, 'rules', units))
      end
    }

    @cellmlEl.delete_element(ruleComponent) if ruleComponent.length == 0
  end

  def process_reactions
    substUnits = @units.findUnits('substance')
    substUnits = Units::Item if substUnits.nil?
    substPerTime = substUnits.merge_with(@units.findUnits('time'), 1.0, -1.0)

    REXML::XPath.each(@sbmlEl, '/sbml/model/listOfReactions/reaction') { |el|
      compname = el.attribute('id').value
      reactionComponent = @cellmlEl.add_element('component', {'name' => compname})

      k = REXML::XPath.first(el, 'kineticLaw/math/' + MathExpressionChild)
      next if k.nil?

      @comps.push_context
      REXML::XPath.each(el, "kineticLaw/listOfParameters/parameter") { |param|
        name = param.attribute('id').value
        attrs = {'name' => name}
        attrs['initial_value'] = param.attribute('value').value unless param.attribute('value').nil?
        if param.attribute('units').nil?
          units = 'unknown'
        else
          units = param.attribute('units').value
        end

        attrs['units'] = units
        
        var = reactionComponent.add_element('variable', attrs)
        @comps.register_variable(name, compname, name, units)

        process_initial_assignment(name, units, reactionComponent, var)
      }
      k = fix_maths(k, compname, substPerTime)
      @comps.pop_context

      [{:stoichFactor => -1, :xpath => 'listOfReactants/speciesReference'},
       {:stoichFactor => 1, :xpath => 'listOfProducts/speciesReference'}].each { |v|
        stoichFactor = v[:stoichFactor]
        REXML::XPath.each(el, v[:xpath]) { |sr|
          speciesName = sr.attribute('species').value
          species = @species[speciesName]
          @comps.connect(speciesName, compname)
          add_math_apply_eq(reactionComponent, speciesName) { |apply|
            stoichConstant = nil
            stoichEl = nil

            eqn = k.deep_clone
            preConversionUnits = substUnits

            if not species[:onlySubstance]
              klaw = eqn
              eqn = REXML::Element.new('apply')
              eqn.add_element('divide')
              eqn.add_element(klaw)
              ci = REXML::Element.new('ci')
              ci.text = species[:compartment][:name]
              @comps.connect(species[:compartment][:name], compname)
              eqn.add_element(ci)
              preConversionUnits = substUnits.merge_with(species[:compartment][:units], 1.0, -1.0)
            end

            units = species[:units]
            unitsFactor = preConversionUnits.conversion_factor(units)
            unitsFactorUnits = units.merge_with(preConversionUnits, 1.0, -1.0)

            if sr.attribute('stoichiometry').nil?
              stoichMath = REXML::XPath.first(sr, 'stoichiometryMath/math/' + MathExpressionChild)
              if stoichMath.nil?
                stoichConstant = 1
              else
                stoichMath = fix_math(stoichMath.deep_clone, compname,
                                      Units::Dimensionless)
                if (stoichFactor * unitsFactorUnits) == 1.0 and unitFactorUnits === Units::Dimensionless
                  stoichEl = stoichMath
                else
                  stoichEl = REXML::Element.new('apply')
                  stoichEl.add_element('times')
                  cn = stoichEl.add_element('cn')
                  cn.text = stoichFactor * unitsFactorUnits
                  cn.add_attribute('cellml:units', @units.findOrMakeUnits(unitsFactorUnits))
                  stoichEl.add_element(stoichMath)
                end
              end
            else
              stoichConstant = sr.attribute('stoichiometry').value.strip.to_f
            end
            if not stoichConstant.nil?
              if (stoichConstant * stoichFactor) != -1
                stoichEl = REXML::Element.new('cn')
                stoichEl.add_attribute('cellml:units', @units.findOrMakeUnits(unitsFactorUnits))
                stoichEl.text = stoichFactor * stoichConstant
              end
            end

            if not stoichEl.nil?
              klaw = eqn
              eqn = REXML::Element.new('apply')
              eqn.add_element('times')
              eqn.add_element(stoichEl)
              eqn.add_element(klaw)
            end

            apply.add_element(eqn)
          }
        }
      }
    }
  end
end

begin
  SBMLToCellML.convert(REXML::Document.new(File.new(ARGV[0]))).write(STDOUT, 1)
  puts ""
rescue SBMLError
  STDERR.puts "Error: %s" % $!.why
end

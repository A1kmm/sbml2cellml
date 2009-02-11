# A caching XPath implementation, to gain us a bit of performance...

require 'rexml/document'

class CachingXPathParser < REXML::XPathParser
  def initialize
    @cache = {}
    super
  end

  def parse path, nodeset  
    path_stack = @cache[path]
    if path_stack.nil? or (not @namespaces.nil?) or (@variables != {})
      path_stack = @parser.parse( path )
      @cache[path] = path_stack
    end

    match( path_stack.dclone, nodeset )
  end
end

class CachingXPath
  def initialize
    @parser = CachingXPathParser.new
  end

  def first element, path=nil, namespaces=nil, variables={}
    raise "The namespaces argument, if supplied, must be a hash object." unless namespaces.nil? or namespaces.kind_of?(Hash)
    raise "The variables argument, if supplied, must be a hash object." unless variables.kind_of?(Hash)
    @parser.namespaces = namespaces
    @parser.variables = variables
    path = "*" unless path
    element = [element] unless element.kind_of? Array
    @parser.parse(path, element).flatten[0]
  end
  
  def each element, path=nil, namespaces=nil, variables={}, &block
    raise "The namespaces argument, if supplied, must be a hash object." unless namespaces.nil? or namespaces.kind_of?(Hash)
    raise "The variables argument, if supplied, must be a hash object." unless variables.kind_of?(Hash)
    @parser.namespaces = namespaces
    @parser.variables = variables
    path = "*" unless path
    element = [element] unless element.kind_of? Array
    @parser.parse(path, element).each( &block )
  end

  # Returns an array of nodes matching a given XPath.
  def match element, path=nil, namespaces=nil, variables={}
    @parser.namespaces = namespaces
    @parser.variables = variables
    path = "*" unless path
    element = [element] unless element.kind_of? Array
    @parser.parse(path,element)
  end
end

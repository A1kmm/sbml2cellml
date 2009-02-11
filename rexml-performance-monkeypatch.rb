# This is a really pragmatic solution to working around some of the slower
# parts of REXML...

require 'rexml/document'

class REXML::Element
  # root is very slow because it requires us to go down many levels. So we cache
  # it. This is only beneficial if we query root lots compared to the number of
  # times we change the model.
  def root
    return @root if defined?(@root) and @rootSerial == $currentREXMLRootSerial

    @rootSerial = $currentREXMLRootSerial
    @root = case
            when self.kind_of?(REXML::Document) then elements[1]
            when (parent.kind_of?(REXML::Document) or parent.nil?) then self
            else parent.root
            end
  end
end

# This is needed to work out when to invalidate the roots. It uses a global,
# which means that different trees affect the performance of each other, but in
# general is the cheapest solution (this is a performance hack after all).
class REXML::Child
  alias :old_set_parent :parent=

  def parent=(other)
    $currentREXMLRootSerial = $currentREXMLRootSerial + 1 if other != @parent
    old_set_parent(other)
  end
end
$currentREXMLRootSerial = 0

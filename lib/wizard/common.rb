module StructuralEquality
  def ==(other)
    self.class == other.class
    self.instance_variables.each do |var|
      v1 = self.instance_variable_get(var)
      v2 = other.instance_variable_get(var)
      return false if v1 != v2
    end
    return true
  rescue NameError => e
    return false
  end
end

hash = { :water => 'wet', :fire => 'hot' }
puts hash[:fire] # Prints:  hot
 
hash.each_pair do |key, value| # Or:  hash.each do |key, value|
  puts "#{key} is #{value}"
end
 
# Prints:  water is wet
#          fire is hot
 
hash.delete :water # Deletes :water => 'wet'
hash.delete_if {|key,value| value=='hot'} # Deletes :fire => 'hot'

# In an object instance variable (denoted with '@'), remember a block.
def remember(&a_block)
  @block = a_block
end
 
# Invoke the above method, giving it a block which takes a name.
remember {|name| puts "Hello, #{name}!"}
 
# When the time is right (for the object) -- call the closure!
@block.call("Jon")
# => "Hello, Jon!"

proc {|arg| print arg}
Proc.new {|arg| print arg}
lambda {|arg| print arg}

class Person
  attr_reader :name, :age
  def initialize(name, age)
    @name, @age = name, age
  end
  def <=>(person) # Comparison operator for sorting
    @age <=> person.age
  end
  def to_s
    "#@name (#@age)"
  end
end
 
group = [
  Person.new("Bob", 33), 
  Person.new("Chris", 16), 
  Person.new("Ash", 23) 
]
 
puts group.sort.reverse
#IRB.conf[:AUTO_INDENT] = true
IRB.conf[:USE_READLINE] = true
IRB.conf[:SAVE_HISTORY] = 5000
# IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
IRB.conf[:HISTORY_FILE] = "#{ENV['XDG_CACHE_HOME']}/irb_history"

IRB.conf[:PROMPT][:MY_PROMPT] = {
      :PROMPT_I => "%N(%m):%03n:%i> ",
      :PROMPT_N => "%N(%m):%03n:%i> ",
      :PROMPT_S => "%N(%m):%03n:%i%l ",
      :PROMPT_C => "%N(%m):%03n:%i* ",
      :RETURN => "=> %s\n"
}
IRB.conf[:PROMPT_MODE] = :MY_PROMPT
#IRB.conf[:PROMPT_MODE] = :SIMPLE

require 'irb/completion'
require 'irb/ext/save-history'
require 'pp'

# load rubygems and wirble
require 'rubygems' rescue nil
require 'wirble'
require 'utility_belt'

# load wirble
Wirble.init
Wirble.colorize


# Now I can type fl 'foo' and then type rl to reload it. Much better.

def fl(file_name)
   file_name += '.rb' unless file_name =~ /\.rb/
   @@recent = file_name 
   load "#{file_name}"
end
 
def rl
  fl(@@recent)
end

# More than one way to do this
# Commented is the ruby way
# uncommentted is my preferred way
def ls
   #entries = instance_eval("Dir.entries(File.dirname(__FILE__))")
   #(entries - ["..", "."]).reverse
   %x{ls}.split("\n")
end


module Misc
  # Reloads a file just as you would require it
  def reload(require_regex)
    $".grep(/#{require_regex}/).each {|e| $".delete(e) && require(e) }
  end

  # Reloads or requires
  def reload_or_require(require_regex)
    require require_regex if reload(require_regex).size.zero?
  end

  # From http://solutious.com/blog/2009/09/22/secret-of-object-to_s/
  # Calculates id found in :to_s of most objects
  def to_s_id(obj)
    "0x%x" % [obj.object_id*2]
  end
end

begin
  # load wirble
  require 'wirble'

  # start wirble (with color)
  Wirble.init
  Wirble.colorize
rescue LoadError => err
  warn "Couldn't load Wirble: #{err}"
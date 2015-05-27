#!/usr/bin/ruby
# encoding: utf-8

require 'rubygems'

infile = File.new('DefaultKeyBinding.dict','r')
input = infile.read
infile.close

date = Time.now.strftime('%m/%d/%Y')

output = ''

intro =<<INTRO
cheatsheet do
  title 'DefaultKeyBinding'
  docset_file_name 'DefaultKeyBinding'
  keyword 'keybinding'

  introduction "DefaultKeyBinding.dict file for Mac OS X, created by [Brett Terpstra](http://brettterpstra.com) and based heavily on work done by [Lri](http://www.cs.helsinki.fi/u/lranta/keybindings/). Please note that these bindings won't work in all applications: TextWrangler and TextMate, for example, override these with their own settings.
See Lri's [gists](https://gist.github.com/Lri) and [website](http://www.cs.helsinki.fi/u/lranta/) for more coding madness.

<b>Documentation</b> <i>(last updated #{date}.)</i>

*Grouped items begin with the groups shortcut (if exists), followed by a subgroup (if exists) followed by the keys specified.*"

INTRO

outro =<<OUTRO
  notes <<-'END'
    * Created by [ttscoff](https://github.com/ttscoff/KeyBindings).
  END
end

OUTRO

toplevel = []

level = 0
prefix = false
group_command = ''
group_desc = ''
subgroup_command = ''
subgroup_desc = ''
desc = ''
command = ''
note = ''

def e_sh(str)
	str.to_s.gsub(/(?=[^a-zA-Z0-9_.\/\-\u007F-\u00FF\n])/, '\\').gsub(/\n/, "'\n'").sub(/^$/, "''")
end

def translate_command(str)
  str = str.gsub(/~/,'⌥').gsub(/@/,'⌘').gsub(/\$/,'⇧')
  str = str.gsub('\UF700','↑').gsub('\UF701','↓').gsub('\UF703','→').gsub('\UF702','←')
  str = str.gsub('\U0009','⇥').gsub('\U000D','↩').gsub('\U001B','⎋').gsub('\U000A','␍')
  str = str.gsub('\UF728','⌦').gsub('\177','⌫')
  str = str.gsub(/([\[\]|])/,"\\\1")
  str = str.gsub(/([A-Z])/,'⇧\\1').downcase
  str
end

input.split("\n").each {|line|

  if line =~ /^\s*$/ || line =~ /^\s*\/\/\s*(TODO)/
    next
  elsif line =~ /^\s*\/\/\s*>\s*(.*)$/
    note += " " + $1
  elsif line =~ /^\s*\};\s*$/
    level -= 1
    if level == 1
      subgroup_command = ''
      subgroup_desc = ''
      output += "\n"
    elsif level == 0
      output += "  end\n\n"
      group_command = ''
      group_desc = ''
    end
    next
  elsif line =~ /^\s*\/\/\s*(.*)/
    desc = $1
    next
  elsif line =~ /^\s*"([^"]+)"\s*=\s*\{.*?\/\/\s*(.*)/
    level += 1
    if level == 1
      group_command = translate_command($1)
      group_desc = $2
      output += "\n  category do\n    id '#{group_desc} (#{group_command})'\n"
    elsif level == 2
      subgroup_command = translate_command($1)
      subgroup_desc = $2
      output += "  category do\n    id '#{subgroup_desc} (#{subgroup_command})'\n"
    else
      prefix = $1
    end
    next
  elsif line =~ /^\s*"([^"]+)"\s*=\s*\(/
    command = translate_command($1)
    note = "(#{note})" if note != ''
    if level == 0

item =<<ITEM
      notes <<-END
        #{note}
      END
ITEM
      tmp  = "    entry do\n"
      tmp += "      name '#{desc}'\n"
      tmp += "      command '"
      tmp += "#{command}".to_s.gsub("'"){"\\'"}
      tmp += "'\n"
      tmp += item
      tmp += "    end\n"
      toplevel.push(tmp)
    else
      command = prefix + "," + command if prefix
item =<<ITEM
      notes <<-END
        #{note}
      END
ITEM
    output += "    entry do\n"
    output += "      name '#{desc}'\n"
    output += "      command '"
    output += "#{group_command} #{subgroup_command} #{command}".to_s.gsub("'"){"\\'"}
    output += "'\n"
    output += item
    output += "    end\n"
    end
    note = ''
  end
}
topoutput = "  category do\n    id 'General Commands'\n"
toplevel.each {|line|
  topoutput += line
}
topoutput += "  end\n\n"

output = topoutput + output

htmlout = %x{echo #{e_sh output}}

outfile = File.new('keybindings.rb','w')
outfile.puts intro
outfile.puts htmlout
outfile.puts outro
outfile.close

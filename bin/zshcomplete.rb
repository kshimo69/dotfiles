#!/usr/bin/env ruby
# $Id: zshcomplete.rb,v 1.4 2007/10/04 02:46:23 rubikitch Exp $
# zsh completion file generator for Ruby/OptionParser
# Save as zshcomplete.rb into a directory in $LOAD_PATH($:).
# (find-sh0 "scp zshcomplete.rb ruby:public_html/archive/zshcomplete.txt")
#
# = Installation
# Add the following to .zshrc: 
# 
# mkdir -p ~/zsh/Completion
# touch ~/zsh/Completion/dummy
# fpath=(~/zsh/Completion $fpath)
# autoload -U ~/zsh/Completion/*(:t)
# autoload -U compinit
# compinit
# generate-complete-function/ruby/optparse () {
#     local cmpl
#     cmpl=~/zsh/Completion/_`basename $1`
#     existp=`[ -f $cmpl ]; echo $?`
#     ruby -rzshcomplete $1 > $cmpl
#     if [ $existp = 0 ]; then
#         reload-complete-functions
#     else
#         compinit
#     fi
# }
# reload-complete-functions() {
#     local f
#     f=(~/zsh/Completion/*(.))
#     unfunction $f:t 2> /dev/null
#     autoload -U $f:t
# }
#
# = Usage
# generate-complete-function/ruby/optparse COMMAND
#
# = Example
# $ generate-complete-function/ruby/optparse =rcov
# $ rcov -[Tab]
# option
# --aggregate                              -- Aggregate data from previous runsin FILE
# --annotate            -a                 -- Generate annotated source code.
# --diff-cmd                               -- Use PROGNAME for --text-coverage-diff.(d
# ...
#
# = History
# $Log: zshcomplete.rb,v $
# Revision 1.4  2007/10/04 02:46:23  rubikitch
# added document
#
# Revision 1.3  2007/10/03 13:31:25  rubikitch
# applied patch in http://subtech.g.hatena.ne.jp/secondlife/20071003/1191394868
#
require 'optparse'

class OptionParser
  ARGV.replace ["--zshcomplete"]
  Officious['zshcomplete'] = proc do |parser|
    Switch::NoArgument.new do
      puts parser.compsys
      exit
    end
  end

  COMPSYS_HEADER = <<'XXX'

typeset -A opt_args
local context state line

_arguments -s -S \
XXX
  
  def compsys
    to = "#compdef #{File.basename($0)}\n"
    to << COMPSYS_HEADER
    visit(:compsys, {}, {}) {|l| to << l}
    to << "  '*:file:_files' && return 0\n"
  end

  class List
    def compsys(*args, &block)
      list.each do |opt|
        if opt.respond_to?(:compsys)
          opt.compsys(*args, &block)
        end
      end
    end
  end

  class Switch
    def compsys(sdone, ldone)
      sopts, lopts, s = [], [], nil
      @short.each {|s| sdone.fetch(s) {sopts << s}; sdone[s] = true} if @short
      @long.each {|s| ldone.fetch(s) {lopts << s}; ldone[s] = true} if @long
      return if sopts.empty? and lopts.empty? # completely hidden

      (sopts+lopts).each do |opt|
        # "(-x -c -r)-l[left justify]" \
        if opt =~ /^--\[no-\](.+)$/
          o = $1
          yield(%Q[  "--#{o}[#{desc.to_s.gsub('"', '\"')}]" \\\n])
          yield(%Q[  "--no-#{o}[#{desc.to_s.gsub('"', '\"')}]" \\\n])
        else
          yield(%Q[  "#{opt}[#{desc.to_s.gsub('"', '\"')}]" \\\n]) # '
        end
      end
    end
  end

end

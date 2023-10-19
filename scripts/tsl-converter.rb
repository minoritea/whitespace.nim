#!/usr/bin/env ruby
$stdout.write $stdin.readlines.map{|line|
  line.split("#").first.chars.reduce("") do |a, c|
    case c
    when "T"; a + "\t"
    when "S"; a + " "
    when "L"; a + "\n"
    else a
    end
  end
}.join

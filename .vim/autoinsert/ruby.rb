# -*- coding: utf-8 -*-
require 'logger'
require 'optparse'

$log = Logger.new(STDOUT)
$log.level = Logger::INFO

opt = OptionParser.new
opt.on('-d') { $log.level = Logger::DEBUG }
opt.parse!(ARGV)

$log.info("start script.")

# CODE

$log.info("end script.")

#!/bin/sh

SCRIPT=${1:-hash.rb}

bundle exec ruby -Ilib "$SCRIPT"
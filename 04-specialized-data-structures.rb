#!/usr/bin/env ruby
# -*- coding: utf-8 -*-


#-------------------------------
# 4.x 
#-------------------------------

# arr = 100.times {|x| arr.push(x+1)}
# arr.bin_search(29)

class Array
  def bin_search(obj)
    if self.length > 0
      finder(obj, 0, self.length - 1)
    end
  end

  def finder(obj, st, ed)
    print "finder=[", obj, " ", st, " ", ed, "]\n"
    range = ed - st;
    if range == 0
      obj == self[st] ? obj : nil
    else
      mid = st + (range / 2).round
      if obj < self[mid]
        finder(obj, st, mid - 1)
      elsif obj > self[mid]
        finder(obj, mid + 1, ed)
      else
        obj
      end
    end
  end
end


#-------------------------------
# 4.x 
#-------------------------------

# parse_date("13 Apr 2014")
# => [13, 4, 2014]

MONTH_NAMES = ["jan", "feb", "mar", "apr", "may", "jun",
               "jul", "aug", "sep", "oct", "nov", "dec"]

class String
  def tokenize
    return self.gsub(/ /, " ").gsub(/,/," ").gsub(/\v/, " ").split(nil)
  end

  def to_month
    MONTH_NAMES.find_index(self.downcase) + 1
  end
end

def parse_date(str)
  toks = str.tokenize
  date = Array.new
  date.push(toks[0].to_i)
  date.push(toks[1].to_month)
  date.push(toks[2].to_i)
end


#-------------------------------
# 4.x 
#-------------------------------

Object.class_eval { remove_const :Node }
Object.class_eval { remove_const :Bst }

class Node
  attr_accessor(:elem, :left, :right)

  def initialize(elem=nil, left=nil, right=nil)
    @elem = elem
    @left = left
    @right = right
  end

  def to_s
    "#<#{@elem}>"
  end
end

class Bst < Node
  def initialize(elem)
    super(elem, nil, nil)
  end

  def insert(elem)
    if @elem == nil
      @elem = elem
    else
      if elem < @elem
        if @left == nil
          @left = Bst.new(elem)
        else
          @left.insert(elem)
        end
      elsif elem > @elem
        if @right == nil
          @right = Bst.new(elem)
        else
          @right.insert(elem)
        end
      end
    end
    self
  end

  def dump(depth=0)
    if @elem == nil
      print (" " * depth), "nil \n"
    else
      print (" " * depth), self.to_s, "\n"
      depth = depth + 2
      if @left == nil
        print (" " * depth), "nil \n"
      else
        @left.dump(depth)
      end
      if @right == nil
        print (" " * depth), "nil \n"
      else
        @right.dump(depth)
      end
    end
  end

  def find(elem)
    if @elem != nil
      if elem < @elem
        @left.find(elem)
      elsif elem > @elem
        @right.find(elem)
      else
        self
      end
    end
  end

  def min
    if @elem != nil
      if @left == nil
        @elem
      else
        @left.min
      end
    end
  end

  def max
    if @elem != nil
      if @right == nil
        @elem
      else
        @right.max
      end
    end
  end

  def remove(elem)
#    print "rm (elem:", @elem, ")\n"
    if @elem != nil
      if elem < @elem
        if @left == nil
          nil
        else
          @left.remove(elem)
        end
      elsif elem > @elem
        if @right == nil
          nil
        else
          @right.remove(elem)
        end
      else
        if @left == nil
          if @right == nil
            @elem = nil
          else
            remove_right(elem)
          end
        elsif @right == nil
          if @left == nil
            @elem = nil
          else
            remove_left(elem)
          end
        else
          if rand(2) == 0
            remove_right(elem)
          else
            remove_left(elem)
          end
        end
      end
    end
  end

  def remove_left(elem)
    @elem = @left.elem
    if @left.left == nil && @left.right == nil
      @left = nil
    else
      @left.remove(@elem)
    end
  end

  def remove_right(elem)
    @elem = @right.elem
    if @right.left == nil && @right.right == nil
      @right = nil
    else
      @right.remove(@elem)
    end
  end
end

bst = Bst.new(nil)
[5, 8, 4, 2, 1, 9, 6, 7, 3].each do |x|
  bst.insert(x)
end

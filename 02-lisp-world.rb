#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

def show_square_iter(st, ed)
  i = st;
  while i <= ed;
    print i, " ", (i * i), "\n"
    i = i + 1;
  end
end

def show_square_recur(st, ed)
  if st <= ed
    print st, " ", (st * st), "\n"
    show_square_recur(st + 1, ed)
  end
end

class Array
  def length_iter
    len = 0;
    self.each do
      len = len + 1
    end
    len
  end
end

class Array
  def length_recur
    f = Proc.new do |arr|
      if arr.empty?
        0
      else
        arr.shift
        1 + f.call(arr)
      end
    end
    f.call(Array.new(self)) # Array#shiftは破壊的メソッドなので
                            # 複製する必要がある
  end
end


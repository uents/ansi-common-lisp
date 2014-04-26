#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

#-------------------------------
# 5.7 例：日付計算
#-------------------------------

# 基点となる年
#  n = 0 は 2000/1/1 となる
YZERO = 2000 

# 年始からの月毎の累積日数
MONTH = lambda do
  accum = 0
  [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31].map do |x|
    accum += x
    accum - x
  end
end.call()

def leap?(y)
  y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)
end

def year_days(y)
  365 + (leap?(y) ? 1 : 0)
end

def date2num(d, m, y)
  (d - 1) + month_num(m, y) + year_num(y)
end

def month_num(m, y)
  MONTH[m-1] + (leap?(y) ? 1 : 0)
end

def year_num(y)
  n = 0
  (y - YZERO).abs.times do |i|
   n += year_days(y + i)
  end
  n
end

def num2date(n)
  y, rest = num_year(n)
  m, d = num_month(rest, y)
  [d, m, y]
end

def num_year(n)
  if n >= 0
    y = YZERO
    accum = 0
    while accum <= n
      accum += year_days(y)
      y += 1
    end
    [y - 1, n - (accum - year_days(y-1))]
  else
    y = YZERO - 1
    accum = -year_days(y)
    while accum > n
      y -= 1
      accum -= year_days(y)
    end
    [y, n - accum]
  end
end

def num_month(n, y)
  nmon = lambda do |n|
    m = MONTH.index {|item| item > n}
    m = 12 if m == nil
    d = n - MONTH[m-1] + 1
    [m, d]
  end

  if leap?(y)
    n == 59 ? [2, 29] : n > 59 ? nmon.call(n-1) : nmon.call(n)
  else
    nmon.call(n)
  end
end


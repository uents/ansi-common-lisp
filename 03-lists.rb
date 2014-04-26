#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# マッピング関数
#  Array#mapは非破壊的、Array#map!は破壊的

[1, 2, 3].map do |i|
  i + 10
end

class String
  def mirror?
    (self == self.reverse) ? true : false
  end
end

# ソート
arr.sort {|x, y| x <=> y} # 昇順にソート
arr.sort {|x, y| y <=> x} # 降順にソート

# 配列のn番目に大きな要素を返すメソッド
class Array
  def nthmost(n)
    arr = Array.new(self)
    arr.sort! {|x, y| x <=> y}
    arr.at(n)
  end
end

# 最短経路

#NET = [[:a, :b, :c], [:b, :c], [:c, :d]]

NET = {:a => [:b, :c],
       :b => [:c],
       :c => [:d]}

class Array
  def second
    arr = Array.new(self)
    arr.shift
    arr
  end
end

def new_paths(path, node, net)
  if net[node] == nil
    []
  else
    net[node].map do |n|
      path + [n]
    end
  end
end

def bfs(ed, queue, net)
  print "queue=", queue, "\n"
  if !queue.empty?
    path = queue.first
    node = path.last
    if node == ed
      path
    else
      bfs(ed, queue.second + new_paths(path, node, net), net)
    end
  end
end

def shortest_path(st, ed, net)
  bfs(ed, [[st]], net)
end


#-------------------------------
# 3.x 練習問題
#-------------------------------

# 3. １つのリストを引数として、おのおのの要素について同じもの（eqlで比較）
#    が出現する回数を示すリストを返す関数を定義せよ
#
# lst = [:a :b :a :d :a :c :d :c :a]
# occurences(lst)
#  => {:a => 4, :c => 2, :d => 2, :b => 1}

lst = [:a, :b, :a, :d, :a, :c, :d, :c, :a]

class Array
  def second
    arr = Array.new(self)
    arr.shift
    arr
  end
end

def occurences_base(result, elem, lst)
  if elem == nil
    result
  else
    if result[elem] == nil
      result[elem] = 1
    else
      result[elem] += 1
    end
    occurences_base(result, lst.first, lstsecond)
  end
end

def occurences(lst)
  occurences_base({}, lst.first, lst.second).sort {|x, y| y[1] <=> x[1]}
end


# 5. 関数pos+は1つのリストを引数として、おのおのの要素にその位置を示す数を加えて返す
#
# lst = [1, 2, 3, 4]
# pos_plus_xxx(lst)
#  => [1, 3, 5, 7] 

# 5-a. 再帰版
class Array
  def second
    arr = Array.new(self)
    arr.shift
    arr
  end
end

def pos_plus_iter_base(result, pos, lst)
  if lst.empty?
    result
  else
    elem = lst.first + pos
    result.push(elem)
    pos_plus_iter_base(result, pos + 1, lst.second)
  end
end

def pos_plus_iter(lst)
  pos_plus_iter_base([], 0, lst)
end

# 5-b. 反復版
def pos_plus_recur(lst)
  result = Array.new(lst)
  result.each_index do |pos|
    result[pos] += pos
  end
end

# 5-c. mapを用いる版
def pos_plus_map(lst)
  pos = -1
  lst.map do |elem|
    elem + (pos += 1)
  end
end


# 9. 3.15節に示したネットワークで最長経路（同じ部分は１度しか通らない）を
#    発見するプログラムを書け。ネットワークは循環部分を含むかもしれない。

NET = {:a => [:b, :c],
       :b => [:a, :c], # 循環あり
       :c => [:d]}

class Array
  def second
    arr = Array.new(self)
    arr.shift
    arr
  end
end

def new_paths_no_loop(path, node, net)
  if net[node] == nil
    []
  else
    net[node].map do |n|
      if path.rindex(n) == nil
        path + [n]
      end
    end.compact
  end
end

def bfs_long(ed, queue, net, result)
  print "queue=", queue, " ret=", result, "\n"
  if queue.empty?
    result
  else
    path = queue.first
    node = path.last
    bfs_long(ed,
             queue.second + new_paths_no_loop(path, node, net),
             net,
             (node == ed) ? path : result)
  end
end

def longest_path(st, ed, net)
  bfs_long(ed, [[st]], net, [])
end


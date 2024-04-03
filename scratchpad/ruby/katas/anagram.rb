# http://codekata.com/kata/kata06-anagrams/

words = File.readlines('wordlist.txt')

groups = {}

words.each do |word|
  word = word.chomp
  begin
    key = word.downcase.split('').sort.join('')
    groups[key] = groups.fetch(key, []) << word
  rescue
    nil
  end
end

File.open('out.txt', 'w') do |f|
  groups.values.each do |e| 
    f.puts e.join(' ')
  end
end

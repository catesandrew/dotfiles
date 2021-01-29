letters = [
  ['A', '🇦', '1f1e6'],
  ['B', '🇧', '1f1e7'],
  ['C', '🇨', '1f1e8'],
  ['D', '🇩', '1f1e9'],
  ['E', '🇪', '1f1ea'],
  ['F', '🇫', '1f1eb'],
  ['G', '🇬', '1f1ec'],
  ['H', '🇭', '1f1ed'],
  ['I', '🇮', '1f1ee'],
  ['J', '🇯', '1f1ef'],
  ['K', '🇰', '1f1f0'],
  ['L', '🇱', '1f1f1'],
  ['M', '🇲', '1f1f2'],
  ['N', '🇳', '1f1f3'],
  ['O', '🇴', '1f1f4'],
  ['P', '🇵', '1f1f5'],
  ['Q', '🇶', '1f1f6'],
  ['R', '🇷', '1f1f7'],
  ['S', '🇸', '1f1f8'],
  ['T', '🇹', '1f1f9'],
  ['U', '🇺', '1f1fa'],
  ['V', '🇻', '1f1fb'],
  ['W', '🇼', '1f1fc'],
  ['X', '🇽', '1f1fd'],
  ['Y', '🇾', '1f1fe'],
  ['Z', '🇿', '1f1ff'],
]

output = ["\\ExplSyntaxOn"]

letters.each do |letter_data|
  output << "\\newunicodechar{#{letter_data[1]}}{ \\flags_print:n {#{letter_data[0]}} }"
end

output << %(
{
\\cs_new:Nn \\flags_print:n
  {
    \\flags_print_unicode:nn { #1 }
  }
}
\\cs_new_protected:Nn \\flags_print_unicode:nn
  {
    \\use:c { flags_#1:n } { #2 }
  }
)

letters.each do |letter_data|
  partial_output = []
  partial_output << %(
\\cs_new_protected:Nn \\flags_#{letter_data[0]}:n
  {
    \\str_case:nnF {#1}
      {
  )
  letters.each do |sub_letter_data|
    partial_output << "{#{sub_letter_data[1]}}{\\xelatexemoji{#{letter_data[2]}-#{sub_letter_data[2]}}}"
  end
  partial_output
  partial_output << %(
      }
      {}
  }
  )
  output << partial_output.join("\n")
end

output << "\\ExplSyntaxOff"

puts output.join("\n")

RSpec.describe Lexer do
  it "lexes characters" do
    l = Lexer.new("  bad beef babe", :str)
    tokens = []
    l.each_token { |t| tokens.push t }
    expected = %w[bad beef babe].map {|w| Ident.new w }
    expect(tokens).to eq(expected)
  end

  it "lexes numbers" do
    l = Lexer.new("  1 2 34", :str)
    tokens = []
    l.each_token { |t| tokens.push t }
    expected = [1, 2, 34].map {|i| IntLiteral.new i }
    expect(tokens).to eq(expected)
  end
end

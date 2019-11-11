RSpec.describe Lexer do
  it "lexes characters" do
    l = Lexer.new("  bad beef babe", :str)
    tokens = []
    l.each_token { |t| tokens.push t }
    expected = %w[bad beef babe].map {|w| Token.new(:ident, w) }
    expect(tokens).to eq(expected)
  end

  it "lexes numbers" do
    l = Lexer.new("  1 2 34", :str)
    tokens = []
    l.each_token { |t| tokens.push t }
    expected = [1, 2, 34].map {|i| Token.new(:int_literal, i) }
    expect(tokens).to eq(expected)

    t = Lexer.new("1.0", :str).lex_token()
    expected = Token.new(:float_literal, 1.0)
    expect(t).to eq (expected)
  end
end

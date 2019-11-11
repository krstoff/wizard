require 'stringio'

require 'wizard/common'

DIGITS_REGEX = /[0-9]/
IDENT_BEGIN_REGEX = /[A-Za-z0-9_]/
IDENT_REGEX = /[a-zA-Z0-9_-]/

class Token
  attr_reader :type, :data
  def initialize(type, data)
    @type = type
    @data = data
  end

  def ==(other)
    self.type == other.type && self.data == other.data
  end
end

class Lexer
  # opts can be :str to lex a string directly
  def initialize(fd, *opts)
    @opts = opts
    stream_class = opts.include?(:str) ? StringIO : File
    @stream = stream_class.open(fd, "r:UTF-8")
    @peek = nil
  end

  def Lexer.tokens(fd, *opts)
    l = Lexer.new(fd, *opts)
    tokens = []
    l.each_token do |t|
      tokens.push(t)
    end
    tokens
  end

  def peek_char()
    @peek ||= @stream.readchar()
  end

  def next_char()
      tmp, @peek = @peek, nil
      tmp or @stream.readchar()
  end

  def each_token()
    begin
      while true
        yield lex_token
      end
    rescue EOFError
    end
  end

  def lex_token()
    # trim whitespace. throws an exception if EOF
    while /\s/.match(peek_char())
      next_char()
    end

    case peek_char()
    when DIGITS_REGEX
      lex_num()
    when IDENT_BEGIN_REGEX
      lex_ident()
    else
      Token.new(:unknown, next_char)
    end
  end

  def lex_ident()
    ident = ""
    begin
      ident << next_char() while IDENT_REGEX =~ peek_char()
    rescue EOFError
    end

    Token.new(:ident, ident)
  end

  def lex_num()
    # Collect digits and return an integer. If there's a period,
    # collect more digits and return a float.
    digits = ""
    period = false
    begin
      digits << next_char() while DIGITS_REGEX =~ peek_char()
      if peek_char() == "."
        period = true
        digits << next_char() while DIGITS_REGEX =~ peek_char()
      end
    rescue EOFError
    end
    if period then Token.new(:float_literal, digits.to_f)
    else Token.new(:int_literal, digits.to_i) end
  end
end

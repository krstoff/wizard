require 'stringio'

require 'wizard/common'

DIGITS_REGEX = /[0-9]/
IDENT_BEGIN_REGEX = /[A-Za-z0-9_]/
IDENT_REGEX = /[a-zA-Z0-9_-]/

class Token
  include StructuralEquality
end

class Ident < Token
  attr_reader :name
  def initialize(name)
    @name = name
  end
end

class IntLiteral < Token
  attr_reader :num
  def initialize(num)
    @num = num
  end
end

class FloatLiteral < Token
  attr_reader :num
  def initialize(num)
    @num = num
  end
end

class Unknown < Token
  attr_reader :char
  def initialize(char)
    @char = char
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
      Unknown.new(next_char)
    end
  end

  def peek_char()
    @peek ||= @stream.readchar()
  end

  def next_char()
      tmp, @peek = @peek, nil
      tmp or @stream.readchar()
  end

  def lex_ident()
    ident = ""
    begin
      ident << next_char() while IDENT_REGEX =~ peek_char()
    rescue EOFError
    end

    Ident.new(ident)
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
    if period then FloatLiteral.new digits.to_f
    else IntLiteral.new digits.to_i end
  end
end

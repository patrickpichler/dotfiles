function nvim 
  if test -n "$NVIM_LISTEN_ADDRESS"
    echo "No vim nesting!"
    exit 1
  end

  command nvim $argv
end

function gradlew --wraps 'gradle'
  if test -x gradlew
    ./gradlew $argv
  else
    echo "No executable file gradlew!"
  end
end

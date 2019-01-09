for ext in cabal hs yaml c h ll s; do
  # removes trailing spaces
  find . -name "*.$ext" -type f -print0 | xargs -0 sed -i 's/[[:space:]]*$//'

  # expand tabs to spaces
  find . -name "*.$ext" ! -type d -exec bash -c 'expand "$0" > /tmp/e && mv /tmp/e "$0"' {} \;
done

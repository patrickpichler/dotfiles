
function edit-nvim-config --description 'Opens nvim in the nvim config folder'
  pushd .

  cd ~/.config/nvim

  nvim .

  popd
end

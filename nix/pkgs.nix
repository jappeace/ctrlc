import ./pin.nix {
  config = {

    packageOverrides = pkgs: {
        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
            ctrlc = hpNew.callPackage ../default.nix {};
            };
        };
    };
  };
}

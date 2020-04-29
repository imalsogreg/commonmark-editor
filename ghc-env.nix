{ reflex-platform, ... }:
  let commonmark-repo =
        #reflex-platform.nixpkgs.fetchFromGitHub
          # { owner = "imalsogreg";
          #   repo  = "commonmark-hs";
          #   rev    = "8f305d36f842b31f2859bceb196a0ad5d3304f74";
          #   sha256 = "1f9ib1vxmcxrizvpn7yc5pixm5csdqcyyflzbwh1v3qr2g1rsf8w";
          # };
          # { owner = "jgm";
          #   repo  = "commonmark-hs";
          #   rev   = "e7f36784374442344d647568e9f2f2a0d10046c9";
          #   sha256 = "1r0hnfr8csgx8agy8gmpfy513mvg7rccqvcww122dd3c3rfrllj1";
          # };
        ../commonmark-hs;
      commonmark = commonmark-repo + "/commonmark";
      commonmark-extensions = commonmark-repo + "/commonmark-extensions";
      emojis-repo = reflex-platform.nixpkgs.fetchFromGitHub
        { owner = "jgm";
          repo = "emojis";
          rev = "74bcd7b19fb572ac920e691c7587665f0c175710";
          sha256 = "0jl6xvfnchkxkwrkrlxzgwn39srscgarpdk3iplr2lwmcz2xr81w";
        };
  in
    reflex-platform.ghc.override {
    overrides = self: super: {
      commonmark =
        self.callCabal2nix "commonmark" commonmark {};
      commonmark-extensions =
        self.callCabal2nix "commonmark-extensions" commonmark-extensions {};
      emojis =
        self.callCabal2nix "emojis" emojis-repo {};
      texmath =
        self.callHackage "texmath" "0.12" {};
      pandoc-types =
        self.callHackage "pandoc-types" "1.20" {};
    };
}

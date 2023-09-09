  systemd.services."update-antizapret" =
    let
      update-antizapret = pkgs.callPackage ./update-antizapret/shell.nix { };
      antizapretCfg = pkgs.writeText "antizapret.json" (builtins.toJSON {
        dns = {
          server = "1.1.1.1";
          threads = 32;
          interval = 600;
        };
        inputs = [
          { source = { 
              type = "feed";
              url = "https://github.com/asert/UA-Blocked-List/commits/master.atom";
              data_url = "https://raw.githubusercontent.com/asert/UA-Blocked-List/master/UA-Blocked-List.txt";
              interval = 10;
            };
            format = "simple";
          }
          { source = {
              type = "filesystem";
              path = "/var/lib/antizapret/local.txt";
            };
            format = "simple";
          }
        ];
        outputs = [
          { sink = {
              type = "filesystem";
              path = "/var/lib/antizapret/ipset.txt";
            };
            filters = [];
            format = { type = "ip_set"; };
          }
          { sink = {
              type = "filesystem";  
              path = "/var/lib/antizapret/pac.js";
            };
            filters = [];
            format = {
              type = "pac";
              proxy = "SOCKS 127.0.0.1:1080";
            };
          }
        ];
      });
    in {
      enable = false;
      description = "Convert zapret-info list to needed formats";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        StateDirectory = "antizapret";
        WorkingDirectory = "/var/lib/antizapret";
        User = "antizapret";
        ExecStart = "${update-antizapret}/bin/update-antizapret ${antizapretCfg}";
      };
    };

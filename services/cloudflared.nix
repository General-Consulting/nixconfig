{
    enable = true;
    tunnels = {
      "00000000-0000-0000-0000-000000000000" = {
        credentialsFile = "~/.config/cloudflared.nixpc";
        ingress = {
          "geoffnix.vteng.io" = {
            service = "http://localhost:8001";
            path = "/*.(jpg|png|css|js)";
          };
        };
        default = "http_status:404";
      };
    };
}

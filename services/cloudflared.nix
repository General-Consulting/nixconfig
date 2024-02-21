{
    enable = true;
    tunnels = {
      "00000000-0000-0000-0000-000000000000" = {
        credentialsFile = "~/.config/cloudflared.nixpc";
        ingress = {
          "*.domain1.com" = {
            service = "http://localhost:80";
            path = "/*.(jpg|png|css|js)";
          };
          "*.domain2.com" = "http://localhost:80";
        };
        default = "http_status:404";
      };
    };
}

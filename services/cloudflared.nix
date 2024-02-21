let tunnel-id = "eedcc6cc-bdc2-44ba-a4d8-23f5d043b2a2";
in
{
    enable = true;
    tunnels = {
      "${tunnel-id}" = {
        credentialsFile = ".cloudflared/${tunnel-id}.js";
        ingress = {
          "tmp1.vteng.io" = {
            service = "http://localhost:8001";
            path = "/*.(jpg|png|css|js|lol)";
          };
        };
        default = "http_status:404";
      };
    };
}

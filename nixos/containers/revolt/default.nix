{ config, webPort, apiPort, wsPort, ... }:
{
  virtualisation.oci-containers.containers."revolt" =
  {
    autoStart = true;
    image = "revoltchat/server:master";
    volumes = [
      "${config.sops.secrets."revolt_vapid_priv".path}:/secrets/vapid_priv"
      "${config.sops.secrets."revolt_vapid_pub".path}:/secrets/vapid_pub"
    ];
    ports = [
      "${toString webPort}:5000"
      "${toString apiPort}:8000"
      "${toString wsPort}:9000"
    ];
    environment = {
      # Webapp
      REVOLT_APP_URL = "http://chat.home.com";
      # API
      REVOLT_PUBLIC_URL = "http://api.chat.home.com";
      VITE_API_URL = "http://api.chat.home.com";
      REVOLT_EXTERNAL_WS_URL = "ws://ws.chat.home.com"; # websocket
      # File server
      AUTUMN_PUBLIC_URL = "http://local.home.com";
      # Metadata proxy
      JANUARY_PUBLIC_URL = "http://local.home.com";
      # Voice chat
      # VOSO_PUBLIC_URL = "https://voso.revolt.chat";

      ## hCaptcha Settings
      REVOLT_UNSAFE_NO_CAPTCHA = "1";
      # REVOLT_HCAPTCHA_KEY = 0x0000000000000000000000000000000000000000;
      # REVOLT_HCAPTCHA_SITEKEY = 10000000-ffff-ffff-ffff-000000000001;

      ## Email Settings
      REVOLT_UNSAFE_NO_EMAIL = "1";
      # SMTP host
      # REVOLT_SMTP_HOST=smtp.example.com;
      # SMTP username
      # REVOLT_SMTP_USERNAME=noreply@example.com;
      # SMTP password
      # REVOLT_SMTP_PASSWORD=CHANGEME;
      # SMTP From header
      # REVOLT_SMTP_FROM=Revolt <noreply@example.com>;

      ## Application Settings
      # Whether to only allow users to sign up if they have an invite code
      REVOLT_INVITE_ONLY = "1";
      # Maximum number of people that can be in a group chat
      REVOLT_MAX_GROUP_SIZE = "100";
      REVOLT_VAPID_PRIVATE_KEY = "LS0tLS1CRUdJTiBFQyBQUklWQVRFIEtFWS0tLS0tCk1IY0NBUUVFSUJSUWpyTWxLRnBiVWhsUHpUbERvcEliYk1yeVNrNXpKYzVYVzIxSjJDS3hvQW9HQ0NxR1NNNDkKQXdFSG9VUURRZ0FFWnkrQkg2TGJQZ2hEa3pEempXOG0rUXVPM3pCajRXT1phdkR6ZU00c0pqbmFwd1psTFE0WAp1ZDh2TzVodU94QWhMQlU3WWRldVovWHlBdFpWZmNyQi9BPT0KLS0tLS1FTkQgRUMgUFJJVkFURSBLRVktLS0tLQo=";
      REVOLT_VAPID_PUBLIC_KEY = "BGcvgR-i2z4IQ5Mw841vJvkLjt8wY-FjmWrw83jOLCY52qcGZS0OF7nfLzuYbjsQISwVO2HXrmf18gLWVX3Kwfw=";

      ## Autumn configuration

      # S3 Region
      AUTUMN_S3_REGION = "minio";
      # S3 Endpoint
      AUTUMN_S3_ENDPOINT = "http://minio:9000";
      # MinIO Root User
      MINIO_ROOT_USER = "minioautumn";
      # MinIO Root Password
      MINIO_ROOT_PASSWORD = "MinIOAutumn";
      # AWS Access Key ID (auto-filled if present above)
      # AWS_ACCESS_KEY_ID=minioautumn;
      # AWS Secret Key (auto-filled if present above)
      # AWS_SECRET_ACCESS_KEY=minioautumn;

      ## Vortex configuration
      # VOSO_MANAGE_TOKEN=CHANGEME;
    };
  };

  sops.secrets."revolt_vapid_priv" = {};
  sops.secrets."revolt_vapid_pub" = {};
}

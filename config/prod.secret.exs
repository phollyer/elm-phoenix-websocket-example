# In this file, we load production configuration and secrets
# from environment variables. You can also hardcode secrets,
# although such is generally not recommended and you have to
# remember to add this file to your .gitignore.
use Mix.Config

secret_key_base =
  System.get_env("EPW_SECRET_KEY_BASE") ||
    raise """
    environment variable EPW_SECRET_KEY_BASE is missing.
    You can generate one by calling: mix phx.gen.secret
    """

config :elm_phoenix_web_socket_example, ElmPhoenixWebSocketExampleWeb.Endpoint,
  http: [
    port: String.to_integer(System.get_env("PORT") || "4001"),
    transport_options: [socket_opts: [:inet6]]
  ],
  secret_key_base: secret_key_base

# ## Using releases (Elixir v1.9+)
#
# If you are doing OTP releases, you need to instruct Phoenix
# to start each relevant endpoint:
#
config :elm_phoenix_web_socket_example, ElmPhoenixWebSocketExampleWeb.Endpoint, server: true
#
# Then you can assemble a release by calling `mix release`.
# See `mix help release` for more information.

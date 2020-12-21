defmodule ElmPhoenixWebSocketExampleWeb.LobbyChannel do
  use Phoenix.Channel

  alias ElmPhoenixWebSocketExample.Room
  alias ElmPhoenixWebSocketExample.User
  alias ElmPhoenixWebSocketExampleWeb.Presence


  def join("example:lobby", %{"username" => username, "background_color" => background_color, "foreground_color" => foreground_color} = params, socket) do
    {:ok, user} =
      User.create(params)
      |> User.update()

    send(self(), :after_join)

    {:ok, user, assign(socket, %{user_id: user.id})}
  end

  def handle_info(:after_join, socket) do
    {:ok, presence} = Presence.track(socket, socket.assigns.user_id, %{
      online_at: System.system_time(:millisecond),
      device: ""
    })

    push(socket, "presence_state", Presence.list(socket))

    push(socket, "room_list", %{rooms: Room.all()})

    {:noreply, socket}
  end

  def terminate(_reason, socket) do
    {:ok, user} = User.find(socket.assigns.user_id)

    Enum.map(user.rooms, &(Room.find(&1)))
      |> Enum.each(fn result ->
        case result do
          {:ok, room} ->
            broadcast(socket, "room_deleted", room)
          :not_found ->
            nil
        end
      end)

    User.delete(user)

    broadcast_room_list(socket)
  end

  def handle_in("create_room", _, socket) do
    {:ok, user} = User.find(socket.assigns.user_id)

    User.create_room(user)

    broadcast_room_list(socket)

    {:reply, :ok, socket}
  end

  def handle_in("delete_room", %{"room_id" => room_id}, socket) do
    {:ok, room} = Room.find(room_id)

    Room.delete(room)

    broadcast_room_list(socket)

    {:reply, :ok, socket}
  end

  def handle_in("room_invite", %{"to" => to_id, "room" => room_id}, socket) do
    {:ok, user} = User.find(socket.assigns.user_id)
    {:ok, room} = Room.find(room_id)

    broadcast(socket, "room_invite", %{to: to_id, from: user, room: room.id})

    {:reply, :ok, socket}
  end
  def handle_in("invite_declined", %{"to" => to_id}, socket) do
    {:ok, user} = User.find(socket.assigns.user_id)

    broadcast(socket, "invite_declined", %{to: to_id, from: user})

    {:reply, :ok, socket}
  end


  defp broadcast_room_list(socket) do
    broadcast(socket, "room_list", %{rooms: Room.all()})
  end
end
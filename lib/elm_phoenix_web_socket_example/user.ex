defmodule ElmPhoenixWebSocketExample.User do

  alias ElmPhoenixWebSocketExample.Room

  def create(username) do
    %{id: create_id(),
      username: username,
      rooms: []
    }
  end

  def find(id) do
    case :ets.lookup(:users_table, id) do
      [{_, user} | _] ->
        {:ok, user}

      [] ->
        :not_found
    end
  end

  def update(user) do
    if :ets.insert(:users_table, {user.id, user}) do
      {:ok, user}
    else
      :error
    end
  end

  def delete(user) do
    Room.delete_list(user.rooms)

   :ets.delete(:users_table, user.id)
  end

  def create_room(user) do
    {:ok, room} =
      Room.create(user)
      |> Room.update()

    Map.update(user, :rooms, [room.id], &([room.id | &1]))
    |> update()
  end

  def create_id(), do: inspect(rem System.system_time(:millisecond), 1_000_000)
end
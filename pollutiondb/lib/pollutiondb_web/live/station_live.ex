defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.getAll(), name: "", lat: "", lon: "", query: "")
    {:ok, socket}
  end

  defp to_name(name) do
    case name do
      "" -> "no_name"
      _ -> name
    end
  end

  defp to_float(val, default) do
    case Float.parse(val) do
      {float, _rest} -> float
      :error -> default
    end
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(%Station{name: to_name(name), lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
    socket = assign(socket, stations: Station.getAll(), name: name, lat: lat, lon: lon)
    {:noreply, socket}
  end

  def handle_event("search", %{"query" => query}, socket) do
    stations =
      Station.getAll()
      |> Enum.filter(fn station ->
        String.contains?(String.downcase(station.name), String.downcase(query))
      end)

    socket = assign(socket, stations: stations, query: query)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
      <style>
        th, td {
          border: 1px solid #ddd;
          padding: 8px;
          text-align: left;
        }
        form input[type="text"],
        form input[type="number"] {
          padding: 6px 8px;
          margin: 5px 0 10px 0;
          width: 200px;
          box-sizing: border-box;
          border: 1px solid #ccc;
          border-radius: 4px;
        }

        form input[type="submit"] {
          padding: 6px 12px;
          background-color: #4CAF50;
          color: white;
          border: none;
          border-radius: 4px;
          cursor: pointer;
        }

        form input[type="submit"]:hover {
          background-color: #45a049;
        }
      </style>

      <h3>Create new station</h3>
      <form phx-submit="insert">
        Name: <input type="text" name="name" value={@name} /><br/>
        Lat: <input type="number" name="lat" step="0.1" value={@lat} /><br/>
        Lon: <input type="number" name="lon" step="0.1" value={@lon} /><br/>
        <input type="submit" />
      </form>

      <h3>Search stations by name</h3>
      <form phx-change="search" autocomplete="off">
        Query: <input type="text" name="query" value={@query} />
      </form>

      <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
              </tr>
              <%= for station <- @stations do %>
                <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
      </table>
    """
  end
end

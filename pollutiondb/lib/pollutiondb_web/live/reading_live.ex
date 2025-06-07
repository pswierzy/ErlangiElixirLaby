defmodule PollutiondbWeb.ReadingLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Reading
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      readings: Reading.find_ten_last,
      date: Date.utc_today |> Date.to_iso8601,
      stations: Station.getAll,
      station_id: nil,
      type: "",
      value: "")
    {:ok, socket}
  end

  defp to_date(""), do: Date.utc_today()

  defp to_date(date_str) do
    case Date.from_iso8601(date_str) do
      {:ok, date} -> date
      {:error, _} -> Date.utc_today()
    end
  end

  def handle_event("search", %{"date" => date_str}, socket) do
    date = to_date(date_str)
    readings = Reading.find_ten_last_by_date(date)

    date_str = if date_str == "", do: Date.to_iso8601(date), else: date_str

    {:noreply, assign(socket, date: date_str, readings: readings)}
  end

  def handle_event("insert", %{"station_id" => station_id, "type" => type, "value" => value}, socket) do
    station = Station.get_by_id(String.to_integer(station_id))

    {fl_val, _} = Float.parse(value)
    Reading.add_now(station, type, fl_val)

    {:noreply, assign(socket, readings: Reading.find_ten_last)}
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

      <form phx-change="search" phx-submit="search">
        <label for="date">Date:</label>
        <input for="date" id="date" name="date" value={@date} />
      </form>

      <table>
      <tr>
        <th>StationName</th><th>Date</th><th>Time</th><th>Type</th><th>Value</th>
              </tr>
              <%= for reading <- @readings do %>
                <tr>
          <td><%= reading.station.name %></td>
          <td><%= reading.date %></td>
          <td><%= reading.time %></td>
          <td><%= reading.type %></td>
          <td><%= reading.value %></td>
        </tr>
      <% end %>
      </table><br/><br/>

      <h3>Add new reading now</h3>
      <form phx-submit="insert">
        <label>Station:</label>
          <select name="station_id">
            <%= for station <- @stations do %>
              <option label={station.name} value={station.id} selected={station.id == @station_id}/>
            <% end %>
          </select><br/>
        <label>Type:</label>
          <input type="text" name="type" value={@type} required /><br/>
        <label>Value:</label>
          <input type="number" name="value" step="0.01" value={@value} required /><br/>

        <input type="submit" value="Add reading" />

      </form>
    """
  end
end

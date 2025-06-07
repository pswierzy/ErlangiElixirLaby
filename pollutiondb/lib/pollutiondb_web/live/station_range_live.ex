defmodule PollutiondbWeb.StationRangeLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, lat_min: 1, lat_max: 4, lon_min: 1, lon_max: 4)
    {:ok, socket}
  end

  def handle_event("update", %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    socket = assign(socket, lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <form phx-change="update">
      Lat min
        <input type="range" min="0" max="5" name="lat_min" value={@lat_min}/><br/>
      Lat max
        <input type="range" min="0" max="5" name="lat_max" value={@lat_max}/><br/>
      Lon min
        <input type="range" min="0" max="5" name="lon_min" value={@lon_min}/><br/>
      Lon max
        <input type="range" min="0" max="5" name="lon_max" value={@lon_max}/><br/>
    </form>
    """
  end

end

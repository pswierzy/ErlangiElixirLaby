defmodule Loading do
  defp parse_datetime(date) do
    date_utc = date
    |> String.slice(0..9)
    |> Date.from_iso8601!

    time_utc = date
    |> String.slice(11..18)
    |> Time.from_iso8601!

    {date_utc, time_utc}
  end

  defp parse_line(line) do
    [datestr, typestr, valuestr, stationIDstr, stationNamestr, locationstr] = line
    {date, time} = parse_datetime(datestr)
    value = String.to_float(valuestr)
    stationID = String.to_integer(stationIDstr)
    {lat, lon} = locationstr
          |> String.split(",")
          |> Enum.map(&String.trim/1)
          |> Enum.map(&String.to_float/1)
          |> List.to_tuple

    %{:date => date,
      :time => time,
      :lat => lat,
      :lon => lon,
      :stationID => stationID,
      :stationName => stationNamestr,
      :pollutionType => typestr,
      :pollutionLevel => value}
  end

  defp identifyStations(data) do
    data
    |> Enum.uniq_by(&(&1.stationID))
    |> Enum.map(&(Map.drop(&1, [:date, :time, :pollutionType, :pollutionLevel])))
  end

  defp loadStations(data) do
    data
    |> Enum.map(&(Pollutiondb.Station.add(&1.stationName, &1.lon, &1.lat)))
  end

  defp loadValues(data) do
    data
    |> Enum.map(&(Pollutiondb.Reading.add(
    hd(Pollutiondb.Station.find_by_name(&1.stationName)),
    &1.date, &1.time, &1.pollutionType, &1.pollutionLevel)))
  end


  def load do
    data = File.read!("C:\\Users\\piotr\\Documents\\GitHub\\ErlangiElixirLaby\\ErlangApp\\pollution_app\\data\\AirlyData-ALL-50k.csv")
           |> String.trim
           |> String.split("\n")
           |> Enum.map(&(String.split(&1, ";")))
           |> Enum.map(& parse_line/1)

    data
    |> identifyStations
    |> loadStations

    data
    |> loadValues
  end
end

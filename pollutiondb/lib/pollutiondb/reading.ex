defmodule Pollutiondb.Reading do
  require Ecto.Query
  require Ecto.Changeset
  use Ecto.Schema

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add(station, date, time, type, value) do
    %Pollutiondb.Station{id: station_id} = station
    %Pollutiondb.Reading{}
    |> changeset(%{date: date, time: time, type: type, value: value, station_id: station_id})
    |> Pollutiondb.Repo.insert
  end

  def add_now(station, type, value) do
    %Pollutiondb.Station{id: station_id} = station
    %Pollutiondb.Reading{}
    |> changeset(%{date: Date.utc_today, time: Time.utc_now, type: type, value: value, station_id: station_id})
    |> Pollutiondb.Repo.insert
  end

  def find_by_date(date) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Reading, date: ^date)
    )
  end

  defp changeset(reading, attrs) do
    reading
    |> Ecto.Changeset.cast(attrs, [:date, :time, :type, :value, :station_id])
    |> Ecto.Changeset.validate_required([:date, :time, :type, :value, :station_id])
    |> Ecto.Changeset.unique_constraint([:date, :time, :type, :station_id], name: :unique_readings_key)
  end

  def find_ten_last() do
    Ecto.Query.from(r in Pollutiondb.Reading,
      limit: 10, order_by: [desc: r.date, desc: r.time])
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end

  def find_ten_last_by_date(date) do
    Ecto.Query.from(r in Pollutiondb.Reading,
      where: r.date == ^date,
      limit: 10,
      order_by: [desc: r.date, desc: r.time])
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end

end

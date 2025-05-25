defmodule Pollutiondb.Station do
  require Ecto.Query
  require Ecto.Changeset
  use Ecto.Schema

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float
    has_many :readings, Pollutiondb.Reading
  end

  def add(station) do
    Pollutiondb.Repo.insert(station)
  end

  def add(name, lon, lat) do
    %Pollutiondb.Station{}
    |> changeset(%{name: name, lon: lon, lat: lat})
    |> Pollutiondb.Repo.insert
  end

  def find_by_name(name) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Station, name: ^name)
    )
  end

  def find_by_location(lon, lat) do
    Pollutiondb.Repo.all(
      Ecto.Query.from(s in Pollutiondb.Station,
                                 where: s.lon == ^lon,
                                 where: s.lat == ^lat)
    )
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon >= ^lon_min,
      where: s.lon <= ^lon_max,
      where: s.lat >= ^lat_min,
      where: s.lat <= ^lat_max)
    |> Pollutiondb.Repo.all
  end

  def update_name(station, newName) do
    changeset(station, %{name: newName})
    |> Pollutiondb.Repo.update
  end

  defp changeset(station, params) do
    station
    |> Ecto.Changeset.cast(params, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(:lon, greater_than_or_equal_to: -180.0, less_than_or_equal_to: 180.0)
    |> Ecto.Changeset.validate_number(:lat, greater_than_or_equal_to: -90.0, less_than_or_equal_to: 90.0)
    |> Ecto.Changeset.unique_constraint(:name)
  end

end

defmodule Gigasecond do
	@doc """
	Calculate a date one billion seconds after an input date.
	"""
	@spec from({pos_integer, pos_integer, pos_integer}) :: :calendar.date

	def from(date) do
    add_one_gigasecond = &(&1 + 1_000_000_000)

    {later, _} = {date, {0, 0, 0}}
                  |> :calendar.datetime_to_gregorian_seconds
                  |> add_one_gigasecond.()
                  |> :calendar.gregorian_seconds_to_datetime
    later
	end
end

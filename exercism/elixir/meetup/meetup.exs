defmodule Meetup do
  @moduledoc """
  Calculate meetup dates.
  """

  @type weekday :: :monday
                 | :tuesday
                 | :wednesday
                 | :thursday
                 | :friday
                 | :saturday
                 | :sunday

  @type schedule :: :first
                  | :second
                  | :third
                  | :fourth
                  | :last
                  | :teenth

  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the
  meetup date should fall.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: :calendar.date

  def meetup(year, month, weekday, :teenth) do
    calc_meetup_date(year, month, weekday, 13..19)
  end

  def meetup(year, month, weekday, :last) do
    fin = :calendar.last_day_of_the_month(year, month)
    start = fin - 6
    calc_meetup_date(year, month, weekday, start..fin)
  end

  def meetup(year, month, weekday, schedule) do
    week_num = case schedule do
      :first  -> 1
      :second -> 2
      :third  -> 3
      :fourth -> 4
      :last   -> 5
    end
    start = week_num * 7 - 6
    fin = Enum.min [(start + 6), :calendar.last_day_of_the_month(year, month)]

    calc_meetup_date(year, month, weekday, start..fin)
  end

  defp calc_meetup_date(year, month, weekday, day_range) do
    to_date = fn day ->
      {{year, month, day}, :calendar.day_of_the_week(year, month, day)}
    end

    {date, _} = Enum.map(day_range, to_date)
                |> Enum.find(fn {_, x} -> to_weekday(x) === weekday end)
    date
  end

  defp to_weekday(daynum) do
    case daynum do
      1 -> :monday
      2 -> :tuesday
      3 -> :wednesday
      4 -> :thursday
      5 -> :friday
      6 -> :saturday
      7 -> :sunday
    end
  end
end

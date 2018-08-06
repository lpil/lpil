class ShoutsController < ApplicationController
  def show
    @shout = Shout.find(params[:id])
  end
end

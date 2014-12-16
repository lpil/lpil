class ShoutsController < ApplicationController
  def create
    current_user
      .shouts.build(shout_parameters)
      .save

    redirect_to dashboard_path
  end

  private

  def shout_parameters
    params.require(:shout).permit(:body)
  end
end

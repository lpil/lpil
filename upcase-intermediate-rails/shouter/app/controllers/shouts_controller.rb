class ShoutsController < ApplicationController
  def show
    @shout = Shout.find(params[:id])
  end

  def create
    shout = current_user.shouts.build(shout_parameters)

    if shout.save
      redirect_to dashboard_path
    else
      flash.alert = 'Could not shout'
      redirect_to dashboard_path
    end
  end

  private

  def shout_parameters
    params.require(:shout).permit(:body)
  end
end

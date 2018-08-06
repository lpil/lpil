class HomesController < ApplicationController
  before_filter :check_logged_in_user

  def show
  end

  private

  def check_logged_in_user
    redirect_to dashboard_path if signed_in?
  end
end

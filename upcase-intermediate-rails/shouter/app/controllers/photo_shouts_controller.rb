class PhotoShoutsController < ApplicationController
  def create
    content = build_content
    shout   = current_user.shouts.build(content: content)

    if shout.save
      redirect_to dashboard_path
    else
      flash.alert = 'Could not shout'
      redirect_to dashboard_path
    end
  end

  private

  def build_content
    PhotoShout.new(photo_shout_parameters)
  end

  def photo_shout_parameters
    params.require(:photo_shout).permit(:image)
  end
end

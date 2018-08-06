class TextShoutsController < ApplicationController
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
    TextShout.new(text_shout_parameters)
  end

  def text_shout_parameters
    params.require(:text_shout).permit(:body)
  end
end

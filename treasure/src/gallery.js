import React from "react";
import LibraryGallery from "react-photo-gallery";
import Lightbox from "react-images";

function toLightboxImages(photos) {
  return photos.map(x =>
    Object.assign({}, x, { srcset: x.srcSet, caption: x.title })
  );
}

export default class Gallery extends React.Component {
  constructor() {
    super();
    this.state = { currentImage: 0 };
    this.closeLightbox = this.closeLightbox.bind(this);
    this.openLightbox = this.openLightbox.bind(this);
    this.gotoNext = this.gotoNext.bind(this);
    this.gotoPrevious = this.gotoPrevious.bind(this);
  }

  openLightbox(event, obj) {
    this.setState({ currentImage: obj.index, lightboxIsOpen: true });
  }

  closeLightbox() {
    this.setState({ currentImage: 0, lightboxIsOpen: false });
  }

  gotoPrevious() {
    this.setState({ currentImage: this.state.currentImage - 1 });
  }

  gotoNext() {
    this.setState({ currentImage: this.state.currentImage + 1 });
  }

  render() {
    return (
      <div>
        <LibraryGallery
          photos={this.props.photos}
          columns={this.props.columns}
          onClick={this.openLightbox}
        />
        <Lightbox
          theme={{
            container: {
              background: "rgba(0, 0, 0, 0.85)"
            }
          }}
          images={toLightboxImages(this.props.photos)}
          backdropClosesModal={true}
          onClose={this.closeLightbox}
          onClickPrev={this.gotoPrevious}
          onClickNext={this.gotoNext}
          currentImage={this.state.currentImage}
          isOpen={this.state.lightboxIsOpen}
          width={1600}
        />
      </div>
    );
  }
}

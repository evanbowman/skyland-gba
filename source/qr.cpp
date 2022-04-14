#include "qr.hpp"
#include "../external/qr/qrcodegen.h"



namespace skyland
{



std::optional<QRCode> QRCode::create(const char* text)
{
    auto qr_data = make_scratch_buffer("qrcode-data-buffer");

    auto temp = make_scratch_buffer("qr-temp-buffer");

    bool ok = qrcodegen_encodeText(text,
                                   (u8*)temp->data_,
                                   (u8*)qr_data->data_,
                                   qrcodegen_Ecc_LOW,
                                   qrcodegen_VERSION_MIN,
                                   qrcodegen_VERSION_MAX,
                                   qrcodegen_Mask_AUTO,
                                   true);
    if (ok) {
        return QRCode(qr_data);
    } else {
        return {};
    }
}



QRCode::QRCode(ScratchBufferPtr qr_data) :
    qr_data_(qr_data)
{
}



bool QRCode::get_module(const Vec2<int>& position) const
{
    return qrcodegen_getModule((u8*)qr_data_->data_, position.x, position.y);
}



QRCode::Sidelength QRCode::size() const
{
    return qrcodegen_getSize((u8*)qr_data_->data_);
}



}
